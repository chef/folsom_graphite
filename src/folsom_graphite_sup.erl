%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Helper macro for declaring children of supervisor
-define(SUP(I, Args), {I, {I, start_link, Args}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(WORKERNL(I, Args), {I, {I, start, Args}, permanent, 5000, worker, [I]}).
-define(MAX_RESTARTS, 10).
-define(MAX_RESTART_WINDOW, 10).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, ?MAX_RESTARTS, ?MAX_RESTART_WINDOW},
          maybe_start_sender_worker(application:get_all_env(folsom_graphite))}}.

get_env(Key, Default) ->
    case application:get_env(folsom_graphite, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

%This is the case that application is not started
maybe_start_sender_worker([]) ->
    lager:debug("No config found, not starting sender or worker"),
    [];
%This is the case that application is started
maybe_start_sender_worker([{included_applications, _} | []]) ->
    lager:debug("No config found, not starting sender or worker"),
    [];
maybe_start_sender_worker(_Message) ->
    GraphiteHost = get_env(graphite_host, "localhost"),
    GraphitePort = get_env(graphite_port, 2003),
    Prefix = get_env(prefix, "folsom"),
    Application = get_env(application, undefined),
    RetryInterval = get_env(retry_interval, 0),
    SendInterval = get_env(send_interval, 10000),
    lager:debug("Prefix ~w", [Prefix]),
    [?WORKER(folsom_graphite_sender, [GraphiteHost, GraphitePort, RetryInterval]),
     ?WORKER(folsom_graphite_worker, [Prefix, Application, SendInterval])].
