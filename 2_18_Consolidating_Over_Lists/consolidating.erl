-module(consolidating).
-include_lib("eunit/include/eunit.hrl").

-export([join/2, concat/1]).

join(Xs, []) ->
    Xs;
join(Xs, Ys) ->
    reverse(prepend(reverse(Xs), Ys)).

%% Prepends first argument list to second arguments list, symbol by symbol.
%% So prepend ("abc", "def") becomes "cdadef".
prepend(Xs, []) ->
    Xs;
prepend(Xs, [Y | Ys]) ->
    prepend([Y|Xs], Ys).

%% Turns a list around
reverse(Xs) ->
    reverse(Xs, []).

reverse([], Acc) ->
    Acc;
reverse([X | Xs], Acc) ->
    reverse(Xs, [X | Acc]).

join_test() ->
    ?assertEqual([], join([], [])),
    ?assertEqual("a", join([], "a")),
    ?assertEqual("ab", join("a", "b")),
    ?assertEqual("Hello World", join("Hello", " World")).

%% Concatenation of "unlimited"" lists into one list
concat(Xs) ->
    concat(Xs, []).

concat([], Acc) ->
    Acc;
concat([X | Xs], Acc) ->
    concat(Xs, join(Acc, X)).

concat_test() ->
    ?assertEqual([], concat([])),
    ?assertEqual([1, 2, 3, 4], concat([[1], [2, 3], [4]])),
    ?assertEqual("goodbye, world", concat(["goo", "dbye,", " ", "world"])).