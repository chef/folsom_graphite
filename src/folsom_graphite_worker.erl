%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_worker).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-compile([export_all]).
-endif.

-record(state, {send_interval :: integer(),
                prefix :: string(),
                extract_fold_fun :: function()
               }).

-define(METER_FIELDS, [{count, "count"},
                       {one, "1MinuteRate"},
                       {five, "5MinuteRate"},
                       {fifteen, "15MinuteRate"},
                       {mean, "meanRate"}]).
-define(HISTOGRAM_FIELDS, [{min, "min"},
                           {max, "max"},
                           {arithmetic_mean, "mean"},
                           {standard_deviation, "stddev"}]).
-define(PERCENTILE_FIELDS, [{75, "75percentile"},
                            {95, "95percentile"},
                            {99, "99percentile"},
                            {999, "999percentile"}]).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1
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

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Config) ->
    SendInterval = proplists:get_value(send_interval, Config),
    ResetMetrics = proplists:get_value(reset_metrics, Config),

    %% This function is used in publish_to_graphite.
    ExtractFoldFun = case ResetMetrics of
        true -> fun extract_value_and_reset/2;
        _ ->    fun extract_value/2
    end,
    State = #state{send_interval = SendInterval,
                   prefix = make_prefix(Config),
                   extract_fold_fun = ExtractFoldFun
                   },
    timer:send_after(SendInterval, publish),
    {ok, State}.

handle_call(Request, _From, State) ->
    lager:info("Unepected message: handle_call ~p", [Request]),
    {noreply, ok, State}.

handle_cast(Msg, State) ->
    lager:info("Unepected message: handle_cast ~p", [Msg]),
    {noreply, State}.

handle_info(publish, #state{send_interval = SendInterval} = State) ->
    ok = publish_to_graphite(State),
    timer:send_after(SendInterval, publish),
    {noreply, State};
handle_info(Info, State) ->
    lager:info("Unepected message: handle_info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec hostname() -> string().
hostname() ->
    {ok, Hostname} = inet:gethostname(),
    folsom_graphite_util:sanitize(Hostname).

-spec publish_to_graphite(#state{}) -> ok.
publish_to_graphite(#state{prefix = Prefix, extract_fold_fun = F}) ->
    Timestamp = make_timestamp(),
    {_, _, Lines} = lists:foldl(F, {Prefix, Timestamp, []}, folsom_metrics:get_metrics_info()),
    folsom_graphite_sender:send(Lines),
    ok.

extract_value({Name, [{type, histogram}]}, {Prefix, Timestamp, Acc}) ->
    Values = folsom_metrics:get_histogram_statistics(Name),
    {_, _, Acc1} = extract_values(Name, Values, ?HISTOGRAM_FIELDS, {Prefix, Timestamp, Acc}),
    Percentiles = proplists:get_value(percentile, Values),
    extract_values(Name, Percentiles, ?PERCENTILE_FIELDS, {Prefix, Timestamp, Acc1});
extract_value({Name, [{type, gauge}]}, {Prefix, Timestamp, Acc}) ->
    Value = folsom_metrics:get_metric_value(Name),
    {Prefix, Timestamp, [folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.value", [Name]), Value})|Acc]};
extract_value({Name, [{type, meter}]}, {Prefix, Timestamp, Acc}) ->
    Values = folsom_metrics:get_metric_value(Name),
    extract_values(Name, Values, ?METER_FIELDS, {Prefix, Timestamp, Acc});
extract_value({Name, [{type, counter}]}, {Prefix, Timestamp, Acc}) ->
    Value = folsom_metrics:get_metric_value(Name),
    {Prefix, Timestamp, [folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.count", [Name]), Value})|Acc]}.

extract_value_and_reset({Name, _} = E, A) ->
    R = extract_value(E, A),
    %% There is a potential race condition here.
    %% Ideally folsom_metrics:delete_metric would
    %% also return the values...
    %% For now this is probably acceptable as it
    %% is probably unlikely to encounter the race
    %% condition.
    folsom_metrics:delete_metric(Name),
    R.

extract_values(Name, Values, Fields, {Prefix, Timestamp, StartAcc}) ->
    {Prefix, Timestamp, lists:foldl(fun({MetricName, PrettyName}, Acc) ->
                                            V = proplists:get_value(MetricName, Values),
                                            [folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.~s",[Name, PrettyName]), V})| Acc]
                                    end,
                                    StartAcc,
                                    Fields)}.

-spec make_timestamp() -> non_neg_integer().
make_timestamp() ->
    {MegaSecs, Secs, _Microsecs} = os:timestamp(),
    integer_to_list(MegaSecs*1000000 + Secs).

-spec make_prefix(Config :: list()) -> string().
make_prefix(Config) ->
    TopPrefix = proplists:get_value(prefix, Config),
    Application = proplists:get_value(application, Config),
    AppendHostname = proplists:get_value(append_hostname, Config),
    Parts = [folsom_graphite_util:sanitize(TopPrefix)]
        ++ case Application of
            undefined -> [];
            _ -> [folsom_graphite_util:sanitize(Application)]
        end
        ++ case AppendHostname of
            true -> [hostname()];
            _ -> []
        end,
    string:join(Parts, ".").
