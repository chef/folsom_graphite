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
    gen_server:cast(?MODULE, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GraphiteHost, GraphitePort]) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort,
                         [binary, {active, false}],
                         ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            State = #state{socket = Socket},
            {ok, State};
        {error, Reason} ->
            Message = lists:flatten(io_lib:format("Could not connect to graphite server on ~s:~B (~w)", [GraphiteHost, GraphitePort, Reason])),
            {stop, Message}
    end.
handle_call(Request, _From, State) ->
    lager:info("Unexpected message: handle_call ~p", [Request]),
    {noreply, ok, State}.

handle_cast({send, Message}, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Message) of
        ok ->
            {noreply, State};
        {error, closed} ->
            lager:info("Socket disconnected unexpectedly."),
            {stop, {shutdown, socket_closed}, State}
    end;
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

