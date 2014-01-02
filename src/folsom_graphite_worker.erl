%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_worker).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {send_interval :: integer(),
                prefix :: string()
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

-export([start_link/3
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

start_link(Prefix, Application, SendInterval) ->
    gen_server:start_link(?MODULE, [Prefix, Application, SendInterval], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Prefix, Application, SendInterval]) ->
    State = #state{send_interval = SendInterval,
                   prefix = prefix(Prefix, Application)
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

-spec publish_to_graphite(#state{}) -> ok.
publish_to_graphite(#state{prefix = Prefix}) ->
    Timestamp = folsom_graphite_util:make_timestamp(),
    Lines = construct_all_lines(Prefix, Timestamp),

    folsom_graphite_sender:send(Lines).

%% construct the set of graphite lines for all metrics
%% in folsom
-spec construct_all_lines(Prefix :: string(),
                          Timestamp :: string()) -> iolist().
construct_all_lines(Prefix, Timestamp) ->
    [extract_value(MetricInfo, Prefix, Timestamp) || MetricInfo <- folsom_metrics:get_metrics_info()].

%% Construct the graphite lines for a single metric.  In the case of
%% more complex metric types (historgram, meter) this could return
%% multiple lines of output
%%
%% NOTE: folsom has added an extra field to the second tuple returned from
%% folsom_metrics:get_all_metrics() containing tags.  We drop it out here
%% so we can support folsom before or after this API change
extract_value({Name, [{type, Type}, _Tags]}, Prefix, Timestamp) ->
    extract_value({Name, [{type, Type}]}, Prefix, Timestamp);

extract_value({Name, [{type, counter}]}, Prefix, Timestamp) ->
    Value = folsom_metrics:get_metric_value(Name),
    folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.count", [Name]), Value});
extract_value({Name, [{type, gauge}]}, Prefix, Timestamp) ->
    Value = folsom_metrics:get_metric_value(Name),
    folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.value", [Name]), Value});
extract_value({Name, [{type, histogram}]}, Prefix, Timestamp) ->
    Values = folsom_metrics:get_histogram_statistics(Name),
    HistogramFields = extract_values(Name, Values, ?HISTOGRAM_FIELDS, Prefix, Timestamp),
    Percentiles = proplists:get_value(percentile, Values),
    [HistogramFields | extract_values(Name, Percentiles, ?PERCENTILE_FIELDS, Prefix, Timestamp)];
extract_value({Name, [{type, meter}]}, Prefix, Timestamp) ->
    Values = folsom_metrics:get_metric_value(Name),
    extract_values(Name, Values, ?METER_FIELDS, Prefix, Timestamp);
extract_value({_Name, [{type, _}]}, _Prefix, _Timestamp) ->
    [].

extract_values(Name, Values, Fields, Prefix, Timestamp) ->
    [ begin
            V = proplists:get_value(MetricName, Values),
            folsom_graphite_util:graphite_format(Prefix, Timestamp, {io_lib:format("~s.~s",[Name, PrettyName]), V})
      end || {MetricName, PrettyName} <- Fields].

-spec append_hostname(Prefix :: string()) -> string().
append_hostname(undefined) ->
    folsom_graphite_util:hostname();
append_hostname(Prefix) ->
    string:join([Prefix, folsom_graphite_util:hostname()], ".").

-spec prefix(Prefix :: string(),
             Application :: string() | undefined) -> string().
prefix(Prefix, undefined) ->
    append_hostname(folsom_graphite_util:sanitize(Prefix));
prefix(Prefix, Application) ->
    append_hostname(string:join([folsom_graphite_util:sanitize(Prefix),
                                 folsom_graphite_util:sanitize(Application)],".")).

