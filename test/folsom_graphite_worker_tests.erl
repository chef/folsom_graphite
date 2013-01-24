
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_worker_tests).

-include_lib("eunit/include/eunit.hrl").

make_prefix_test_() ->
  [{"Top prefix only",
    fun() -> Config = [{prefix, "1"}],
             ?assertEqual("1", lists:flatten(folsom_graphite_worker:make_prefix(Config)))
    end},
   {"Top and app",
    fun() -> Config = [{prefix, "1"}, {application, "2"}],
             ?assertEqual("1.2", lists:flatten(folsom_graphite_worker:make_prefix(Config)))
    end},
   {"Top and append hostname",
    fun() -> Config = [{prefix, "1"}, {append_hostname, true}],
             Expected = string:join(["1", folsom_graphite_worker:hostname()], "."),
             ?assertEqual(Expected, lists:flatten(folsom_graphite_worker:make_prefix(Config)))
    end},
   {"Top, app, and append hostname",
    fun() -> Config = [{prefix, "1"}, {application, "2"}, {append_hostname, true}],
             Expected = string:join(["1", "2", folsom_graphite_worker:hostname()], "."),
             ?assertEqual(Expected, lists:flatten(folsom_graphite_worker:make_prefix(Config)))
    end}
  ].
