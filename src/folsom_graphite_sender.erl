%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_sender).

-behaviour(gen_server).

-define(CONNECT_TIMEOUT, 2000).

-record(state, {socket :: inet:socket(),
                host, port, retry_interval}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3,
         send/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(GraphiteHost, GraphitePort, RetryInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GraphiteHost, GraphitePort, RetryInterval], []).

send([]) ->
    ok;
send(Message) ->
    gen_server:call(?MODULE, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GraphiteHost, GraphitePort, RetryInterval]) ->
    State = #state{host = GraphiteHost, port = GraphitePort, retry_interval = RetryInterval},
    case connect(State) of
        #state{socket = undefined, retry_interval = 0} ->
            % Only reconnect when retry_interval is set
            {stop, {error, failed_connect}};
        State2 ->
            {ok, State2}
    end.

handle_call({send, _Message}, _From, #state{socket = undefined} = State) ->
    {reply, ok, State};
handle_call({send, Message}, _From, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Message) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            case Reason of
                timeout ->
                    lager:error("Timed out sending metrics to graphite.");
                closed ->
                    lager:error("Socket disconnected unexpectedly.");
                Reason ->
                    lager:error("Unexpected error occurred while sending data to graphite: ~p~n", [Reason])
            end,
            error_reply(State)
    end;
handle_call(Request, _From, State) ->
    lager:info("Unexpected message: handle_call ~p", [Request]),
    {noreply, ok, State}.

handle_cast(Msg, State) ->
    lager:info("Unexpected message: handle_cast ~p", [Msg]),
    {noreply, State}.

handle_info(reconnect, #state{socket = undefined} = State) ->
    {noreply, connect(State)};
handle_info(Info, State) ->
    lager:info("Unexpected message: handle_info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined ->
            ok;
        _Else ->
            gen_tcp:close(Socket)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
connect(#state{host = Host, port = Port, retry_interval = Interval} = State) ->
    case gen_tcp:connect(Host, Port,
                         [binary, {packet, raw}, {active, false},
                          {keepalive, true}, {nodelay, true},
                          {send_timeout, 2000}], ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            lager:info("Connection to graphite server successful."),
            State#state{socket = Socket};
        {error, Reason} ->
            lager:error("Failed to connect to graphite server on ~s:~B: ~p~n", [Host, Port, Reason]),
            erlang:send_after(Interval, self(), reconnect),
            State#state{ socket = undefined }
    end.

error_reply(#state{retry_interval = 0} = State) ->
    {stop, {shutdown, send_error}, State};
error_reply(#state{retry_interval = Interval} = State) ->
    erlang:send_after(Interval, self(), reconnect),
    {reply, ok, State#state{socket = undefined}}.

