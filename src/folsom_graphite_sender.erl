%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_sender).

-behaviour(gen_server).

-define(CONNECT_TIMEOUT, 2000).

-record(state, { socket :: inet:socket() }).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
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

start_link(GraphiteHost, GraphitePort) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GraphiteHost, GraphitePort], []).

send(Message) ->
    gen_server:call(?MODULE, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GraphiteHost, GraphitePort]) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort,
                         [binary, {packet, raw}, {active, false},
                          {keepalive, true}, {nodelay, true},
                          {send_timeout, 2000}], ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            State = #state{socket = Socket},
            {ok, State};
        {error, Reason} ->
            lager:error("Failed to connect to graphite server on ~s:~B: ~p~n", [GraphiteHost, GraphitePort, Reason]),
            {stop, {error, failed_connect}}
    end.
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
            {stop, {shutdown, send_error}, State}
    end;
handle_call(Request, _From, State) ->
    lager:info("Unexpected message: handle_call ~p", [Request]),
    {noreply, ok, State}.

handle_cast(Msg, State) ->
    lager:info("Unexpected message: handle_cast ~p", [Msg]),
    {noreply, State}.

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

