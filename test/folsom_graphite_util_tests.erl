
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_util_tests).

-include_lib("eunit/include/eunit.hrl").

sanitize_test_() ->
    [{"test with no subs",
      fun() -> Got = folsom_graphite_util:sanitize(<<"foobar">>),
               ?assertEqual("foobar", Got)
            end},
     {"test with binary",
      fun() -> Got = folsom_graphite_util:sanitize(<<"foo.bar.com">>),
               ?assertEqual("foo_bar_com", Got)
            end},
    {"test with string",
      fun() -> Got = folsom_graphite_util:sanitize("foo.bar.com"),
               ?assertEqual("foo_bar_com", Got)
            end}].

remove_spaces_test_() ->
    [{"test with no subs",
      fun() -> Got = folsom_graphite_util:remove_spaces(<<"foobar">>),
               ?assertEqual("foobar", Got)
            end},
     {"test with binary",
      fun() -> Got = folsom_graphite_util:remove_spaces(<<"foo bar com">>),
               ?assertEqual("foo_bar_com", Got)
            end},
    {"test with string",
      fun() -> Got = folsom_graphite_util:remove_spaces("foo bar com"),
               ?assertEqual("foo_bar_com", Got)
            end}].

concat_test_() ->
    [{"No items returns \n",
      fun() -> Got = folsom_graphite_util:concat([]),
               ?assertEqual("\n", lists:flatten(Got))
            end},
     {"One item gets \n added",
      fun() -> Got = folsom_graphite_util:concat(["foo"]),
               ?assertEqual("foo\n", lists:flatten(Got))
            end},
     {"two items concatenated correctly",
      fun() -> Got = folsom_graphite_util:concat(["foo", "bar"]),
               ?assertEqual("foo bar\n", lists:flatten(Got))
            end},
     {"three items concatenated correctly",
      fun() -> Got = folsom_graphite_util:concat(["foo", "bar", "baz"]),
               ?assertEqual("foo bar baz\n", lists:flatten(Got))
            end}
    ].
