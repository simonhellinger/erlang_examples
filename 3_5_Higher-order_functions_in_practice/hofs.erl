-module(hofs).
-include_lib("eunit/include/eunit.hrl").

-export([double_all/1, evens/1]).


double_all(Xs) -> lists:map(fun (X) -> X * 2 end, Xs).

evens(Xs) -> lists:filter(fun (X) -> X rem 2 == 0 end, Xs).

product(Xs) -> lists:foldr(fun(X, Prod) -> Prod * X end, 1, Xs).

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



% TODO: try property-based testing
