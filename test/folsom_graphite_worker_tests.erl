
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@getchef.com>
%% @copyright 2013 Chef Inc.

-module(folsom_graphite_worker_tests).

-include_lib("eunit/include/eunit.hrl").

append_hostname_test_() ->
    MockedModules = [ folsom_graphite_util ],
    {foreach,
     fun() -> test_util:setup(MockedModules),
              meck:expect(folsom_graphite_util, hostname,
                          fun() -> "my_host" end)
        end,
     fun(_) -> test_util:cleanup(MockedModules)
        end,
     [
     {"hostname is appended",
      fun() -> ?assertEqual("foo.my_host",
                            folsom_graphite_worker:append_hostname("foo")),
               ?assertEqual("bar.my_host",
                            folsom_graphite_worker:append_hostname("bar")),
               ?assertEqual("foo.bar.my_host",
                            folsom_graphite_worker:append_hostname("foo.bar"))

                end
     },
     {"handles undefined prefix",
      fun() -> ?assertEqual("my_host",
                            folsom_graphite_worker:append_hostname(undefined))
                end
     }
     ]
    }.

prefix_test_() ->
    MockedModules = [ folsom_graphite_util ],
    {foreach,
     fun() -> test_util:setup(MockedModules),
              meck:expect(folsom_graphite_util, hostname,
                          fun() -> "my_host" end),
              meck:expect(folsom_graphite_util, sanitize,
                          fun(Str) -> Str end)
        end,
     fun(_) -> test_util:cleanup(MockedModules)
        end,
     [
     {"handles undefined application",
      fun() -> ?assertEqual("foo.my_host",
                            folsom_graphite_worker:prefix("foo", undefined))
                end
     },
     {"handles prefixing with an application",
      fun() -> ?assertEqual("foo.bar.my_host",
                            folsom_graphite_worker:prefix("foo", "bar"))
                end
     }
     ]
    }.

construct_metric_line_test_() ->
    Prefix = "prefix",
    TS = "12345678",
    ExpectedFun = fun(Key, Value) ->
            list_to_binary(io_lib:format("~s.~s ~w ~s~n", [Prefix, Key, Value, TS]))
    end,
    TestLineFun = fun(Name, Value, Type, Expected) ->
            folsom_metrics:notify(Name, Value, Type),
            [M] = folsom_metrics:get_metric_info(Name),
            Line = folsom_graphite_worker:construct_metric_line(M, Prefix, TS),
            ?assertEqual(Expected, list_to_binary(Line))
    end,
    {foreach,
     fun() -> setup([]) end,
     fun teardown/1,
     [
        {"construct_metric_line for a Counter",
         fun() -> Expected = ExpectedFun("count1.count", 1),
                  TestLineFun("count1", {inc, 1}, counter, Expected)
                end
        },
        {"construct_metric_line for a Gauge",
         fun() -> Expected = ExpectedFun("gauge1.value", 1),
                  TestLineFun("gauge1", 1, gauge, Expected)
                end
        },
        {"construct_metric_line does not error on History",
         fun() -> Expected = <<>>,
                  TestLineFun("histo", "something happened", history, Expected)
                end
        }

    ]}.

construct_all_lines_test_() ->
    Prefix = "prefix",
    TS = "12345678",
    ExpectedFun = fun(Key, Value) ->
            list_to_binary(io_lib:format("~s.~s ~w ~s~n", [Prefix, Key, Value, TS]))
    end,
    TestLinesFun = fun(Metrics, ExpectedLines) ->
            NewLine = <<"\n">>,
            [ folsom_metrics:notify(Name, Value, Type) || {Name, Value, Type} <- Metrics],
            Lines = folsom_graphite_worker:construct_all_lines(Prefix, TS),
            SplitLines = binary:split(list_to_binary(Lines), NewLine, [global, trim]),
            [?assertEqual(Expected, <<Line/binary,NewLine/binary>>) ||
                {Line, Expected} <- lists:zip(SplitLines, ExpectedLines)]
    end,
    {foreach,
     fun() -> setup([]) end,
     fun teardown/1,
     [
        {"construct_all_lines works for one metric",
         fun() -> Expected = [ ExpectedFun("gauge1.value", 1)],
                  TestLinesFun([{"gauge1", 1, gauge}], Expected)
                end
        },
        {"construct_all_lines works for more than 1 metric",
         fun() -> Metrics = [{"gauge1", 1, gauge},
                             {"gauge33", 123, gauge},
                             {"my_counter1", {dec, 2}, counter}],
                  Expected = [ExpectedFun("gauge1.value", 1),
                              ExpectedFun("gauge33.value", 123),
                              ExpectedFun("my_counter1.count", -2)],
                  TestLinesFun(Metrics, Expected)
                end
        },
        {"Can run construct_all_lines for complex metrics",
         %% Hard to test this since we can't work out the values so well -
         %% Let's just make sure we can run construct_all_metrics where
         %% we've got complex metrics
         fun() -> Metrics = [{"gauge1", 1, gauge},
                             {"gauge33", 123, gauge},
                             {"my_counter1", {dec, 2}, counter},
                             {"histo1", 1, histogram},
                             {"dur1", timer_start, duration},
                             {"dur1", timer_end, duration},
                             {"meter1", 1, meter}],
                  [folsom_metrics:notify(Name, Value, Type) || {Name, Value, Type} <- Metrics],
                  Lines = folsom_graphite_worker:construct_all_lines(Prefix, TS),
                  ?assertNot([] =:= Lines)
                end
        }
    ]}.

%%
%% Internal functions
%%
setup(Modules) ->
    test_util:setup(Modules),
    application:start(folsom),
    Modules.

teardown(Modules) ->
    application:stop(folsom),
    test_util:cleanup(Modules).

