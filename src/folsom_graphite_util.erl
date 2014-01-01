%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode Inc.

%% @doc Utility functions for dealing with graphite metrics

-module(folsom_graphite_util).

-export([sanitize/1,
         remove_spaces/1,
         graphite_format/3,
         to_list/1
        ]).

%% the various types from folsom key/values
-type folsom_type() :: atom() | list() | binary() | number().

-ifdef(TEST).
-compile([export_all]).
-endif.

-spec sanitize(Str :: string() | binary()) -> string().
%% @doc Cleanup aribitrary strings for use in
%% graphite metrics names - no "."
sanitize(Str) ->
    do_substitution(Str, ".", "_").

-spec remove_spaces(Str :: string()| binary()) -> string().
%% @doc Remove spaces for graphite keys
remove_spaces(Str) ->
    do_substitution(Str, " ", "_").

do_substitution(Str, From, To) when is_binary(Str) ->
    do_substitution(binary_to_list(Str), From, To);
do_substitution(Str, From, To) ->
    string:join(string:tokens(Str, From), To).


-spec graphite_format(Prefix :: string(),
                      Timestamp :: string(),
                      {Key :: folsom_type(), Value :: folsom_type()}) -> iolist().
%% Convert a metric into a line suitable for graphite.
%%
%% We use the mappings defined in codahales metrics GraphiteReporter
%% (http://bit.ly/UFprFV)
%%
%% Returns an iolist
graphite_format(Prefix, Timestamp, {Key, Value}) ->
    MetricName = string:join([Prefix, to_list(Key)], "."),
    concat([remove_spaces(MetricName),
            to_list(Value),
            Timestamp]).



%% join up the elements of the graphite output line, separated
%% by spaces and suffixed by a newline
concat(Lst) ->
    lists:reverse(concat(Lst, [])).
concat([], Acc) -> ["\n" | Acc];
concat([H], Acc) -> concat([], [H | Acc]);
concat([H|T], Acc) ->
    concat(T, [" " | [ H | Acc]]).

-spec to_list(folsom_type()) -> list().
%% Helper to convert arbitrary types we see in folsom metric
%% values into list().
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_float(V) ->
    io_lib:format("~w", [V]);
to_list(V) when is_integer(V) ->
    integer_to_list(V).


