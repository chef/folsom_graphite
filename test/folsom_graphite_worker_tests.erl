
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

extract_value_test_() ->
    MockedModules = [],
    Prefix = "prefix",
    TS = "12345678",
    ExpectedFun = fun(Key, Value) ->
            list_to_binary(io_lib:format("~s.~s ~w ~s~n", [Prefix, Key, Value, TS]))
    end,
    TestLineFun = fun(Name, Value, Type, Expected) ->
            folsom_metrics:notify(Name, Value, Type),
            [M] = folsom_metrics:get_metric_info(Name),
            Line = folsom_graphite_worker:extract_value(M, Prefix, TS),
            ?assertEqual(Expected, list_to_binary(Line))
    end,
    {foreach,
     fun() -> test_util:setup(MockedModules),
              application:start(folsom)
        end,
     fun(_) -> application:stop(folsom),
               test_util:cleanup(MockedModules)
        end,
     [
        {"extract_value for a Counter",
         fun() -> Expected = ExpectedFun("count1.count", 1),
                  TestLineFun("count1", {inc, 1}, counter, Expected)
                end
        },
        {"extract_value for a Gauge",
         fun() -> Expected = ExpectedFun("gauge1.value", 1),
                  TestLineFun("gauge1", 1, gauge, Expected)
                end
        }
    ]}.

