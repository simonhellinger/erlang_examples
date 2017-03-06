-module(hofs).
-include_lib("eunit/include/eunit.hrl").

-export([double_all/1, evens/1, product/1, zip/2, zip_with/3, zip_with_hof/3, zip_hof/2]).


double_all(Xs) -> lists:map(fun (X) -> X * 2 end, Xs).

evens(Xs) -> lists:filter(fun (X) -> X rem 2 == 0 end, Xs).

product(Xs) -> lists:foldr(fun(X, Prod) -> Prod * X end, 1, Xs).

% zip/2 without using higher order functions
zip([], _Ys) ->
    [];
zip(_Xs, []) ->
    [];
zip([X|Xs], [Y|Ys]) ->
    [{X, Y} | zip(Xs, Ys)].

% zip_with3, without using higher order functions
zip_with(_F, [], _Ys) ->
    [];
zip_with(_F, _Xs, []) ->
    [];
zip_with(F, [X|Xs], [Y|Ys]) ->
    [F(X, Y) | zip_with(F, Xs, Ys)].

% zip_with_hof is zip_with redefined, uses map and zip
zip_with_hof(F, Xs, Ys) ->
    lists:map(fun ({X, Y}) -> F(X, Y) end, zip(Xs, Ys)).

% zip_hof is zip redefined. This seems to become unnecessarily roundabout.
zip_hof(Xs, Ys) ->
    zip_with_hof(fun (X, Y) -> {X, Y} end, Xs, Ys).

double_all_test()->
    ?assertEqual([], double_all([])),
    ?assertEqual([0], double_all([0])),
    ?assertEqual([2, 4, 6], double_all([1, 2, 3])).

evens_test() ->
    ?assertEqual([],evens([])),
    ?assertEqual([],evens([1, 3, 5])),
    ?assertEqual([0, 2, 4],evens([0, 1, 2, 3, 4])).

product_test() ->
    ?assertEqual(1 ,product([])),
    ?assertEqual(0 ,product([0, 1, 2])),
    ?assertEqual(24 ,product([1, 2, 3, 4])).

zip_test() ->
    ?assertEqual([], zip([],[])),
    ?assertEqual([], zip([1, 2, 3],[])),
    ?assertEqual([], zip([],[1, 2, 3])),
    ?assertEqual([{1, 4}, {2, 5}], zip([1, 2, 3],[4, 5])),
    ?assertEqual([{1, 4}, {2, 5}], zip([1, 2],[4, 5, 3])).

zip_with_test() ->
    F = fun (X, Y) -> X * Y end,
    ?assertEqual([], zip_with(F, [], [])),
    ?assertEqual([], zip_with(F, [1, 2, 3], [])),
    ?assertEqual([], zip_with(F, [], [1, 2, 3])),
    ?assertEqual([4, 10], zip_with(F, [1, 2, 3], [4, 5])),
    ?assertEqual([4, 10], zip_with(F, [1, 2], [4, 5, 3])).

zip_with_hof_test() ->
    F = fun (X, Y) -> X * Y end,
    ?assertEqual([], zip_with_hof(F, [], [])),
    ?assertEqual([], zip_with_hof(F, [1, 2, 3], [])),
    ?assertEqual([], zip_with_hof(F, [], [1, 2, 3])),
    ?assertEqual([4, 10], zip_with_hof(F, [1, 2, 3], [4, 5])),
    ?assertEqual([4, 10], zip_with_hof(F, [1, 2], [4, 5, 3])).

zip_hof_test() ->
    ?assertEqual([], zip_hof([],[])),
    ?assertEqual([], zip_hof([1, 2, 3],[])),
    ?assertEqual([], zip_hof([],[1, 2, 3])),
    ?assertEqual([{1, 4}, {2, 5}], zip_hof([1, 2, 3],[4, 5])),
    ?assertEqual([{1, 4}, {2, 5}], zip_hof([1, 2],[4, 5, 3])).
