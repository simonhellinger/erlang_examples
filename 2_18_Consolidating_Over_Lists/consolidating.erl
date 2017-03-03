-module(consolidating).
-include_lib("eunit/include/eunit.hrl").

-export([join/2, concat/1]).

%% joins two lists (and, thus, strings) together.
join(Xs, []) ->
    Xs;
join(Xs, Ys) ->
    reverse(prepend(reverse(Xs), Ys)).

% Prepends first argument list to second arguments list, symbol by symbol.
% So prepend ("abc", "def") becomes "cdadef".
prepend(Xs, []) ->
    Xs;
prepend(Xs, [Y | Ys]) ->
    prepend([Y|Xs], Ys).

% Turns a list around
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

%% Concatenation of "unlimited"" lists into one list.
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

%% Checks whether an element is in a list
member(_, []) ->
    false;
member(N, [X|Xs]) ->
    case N of
        X -> 
            true;
        _ ->
            member(N, Xs)
    end.

member_test() ->
    ?assertEqual(false, member(1, [])),
    ?assertEqual(true, member(1, [1])),
    ?assertEqual(false, member(2, [1])),
    ?assertEqual(true, member(2, [1, 3, 4, 2, 6])),
    ?assertEqual(true, member([3, 4], [1, 2, [3, 4], 5])).


insertion_sort([]) ->
    [];
insertion_sort([X|Xs]) ->
    insert(X, insertion_sort(Xs)).

insert(X, []) ->
    [X];
insert(X, [Y|Ys] = A) ->
    case X < Y of
        true ->
            [X| A];
        false ->
            [Y | insert(X, Ys)]
    end.

insertion_sort_test() ->
    ?assertEqual([], insertion_sort([])),
    ?assertEqual([1], insertion_sort([1])),
    ?assertEqual([1, 2], insertion_sort([2, 1])),
    ?assertEqual([1, 2, 3, 4, 5, 6, 7, 70, 900], insertion_sort([900, 1, 70, 4, 2, 3, 5, 6, 7])).