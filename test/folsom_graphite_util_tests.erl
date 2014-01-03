
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

-module(folsom_graphite_util_tests).

-include_lib("eunit/include/eunit.hrl").

timestamp_test() ->
    T1 = folsom_graphite_util:make_timestamp(),
    ?assert(is_list(T1)),
    timer:sleep(1000),
    T2 = folsom_graphite_util:make_timestamp(),
    ?assert(list_to_integer(T1) + 1 == list_to_integer(T2)).

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
            end},
     {"test with undefined",
      fun() -> Got = folsom_graphite_util:sanitize(undefined),
               ?assertEqual(undefined, Got)
            end}
    ].

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

%% Not much we can check, other than it's a string that is returned
hostname_test() ->
    H = folsom_graphite_util:hostname(),
    ?assert(is_list(H)).

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


graphite_format_test_helper(ExpectedKey, Key, ExpectedValue, Value) ->
    TS = "12345678",
    Prefix = "prefix",
    fun() ->
            Expected = [Prefix ++ "." ++ ExpectedKey, " ", ExpectedValue, " ", TS, "\n"],
            ?assertEqual(Expected,
                         folsom_graphite_util:graphite_format(Prefix, TS, {Key, Value}))
    end.

graphite_format_key_test_() ->
    TestData = [{"val", "val", "string"},
                {"val_with_spaces", "val with spaces", "string with spaces"},
                {"1.0", 1.0, "float"},
                {"123", 123, "number"},
                {"-123", -123, "negative number"},
                {"foo_bar", foo_bar, "atom"},
                {"foo.bar", 'foo.bar', "atom with ."},
                {"foo_bar", foo_bar, "atom"},
                {"foo", <<"foo">>, "binary"},
                {"foo_bar", <<"foo bar">>, "binary with spaces"}
               ],
    [{"graphite_format concatenates properly for key with " ++ Description,
      graphite_format_test_helper(ExpectedKey, Key, "val", "val")
     } || {ExpectedKey, Key, Description} <- TestData
    ].

graphite_format_value_test_() ->
    TestData = [{"val", "val", "string"},
                {"val with spaces", "val with spaces", "string with spaces"},
                {"1.0", 1.0, "float"},
                {"123", 123, "number"},
                {"-123", -123, "negative number"},
                {"foo_bar", foo_bar, "atom"},
                {"foo.bar", 'foo.bar', "atom with ."},
                {"foo_bar", foo_bar, "atom"},
                {"foo", <<"foo">>, "binary"},
                {"foo bar", <<"foo bar">>, "binary with spaces"}
               ],
    [{"graphite_format concatenates properly for value with " ++ Description,
      graphite_format_test_helper("key", "key", ExpectedVal, Val)
     } || {ExpectedVal, Val, Description} <- TestData
    ].
