%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

%% Helper macro for declaring children of supervisor
-define(SUP(I, Args), {I, {I, start_link, Args}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(WORKERNL(I, Args), {I, {I, start, Args}, permanent, 5000, worker, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    GraphiteHost = get_env(graphite_host, "localhost"),
    GraphitePort = get_env(graphite_port, 2003),
    Prefix = get_env(prefix, "folsom"),
    Application = get_env(application, undefined),
    SendInterval = get_env(send_interval, 10000),
    ResetMetrics = get_env(reset_metrics, false),
    lager:debug("Prefix ~w", [Prefix]),
    {ok, {{one_for_one, 10, 3600},
          [?WORKER(folsom_graphite_sender, [GraphiteHost, GraphitePort]),
           ?WORKER(folsom_graphite_worker, [Prefix, Application, SendInterval, ResetMetrics])]
         }}.

get_env(Key, Default) ->
    case application:get_env(folsom_graphite, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
