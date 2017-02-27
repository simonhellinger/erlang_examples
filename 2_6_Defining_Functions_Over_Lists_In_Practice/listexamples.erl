-module(listexamples).
-include_lib("eunit/include/eunit.hrl").

-export([productTail/1, productDirect/1, maxTail/1, maxDirect/1]).

% tail recursion of product
productTail(Xs) ->
    productTail(Xs, 1). % Acc defaults to 1, since 1 is the neutral element for products.

productTail([], Acc) ->
    Acc;
productTail([H|T], Acc) ->
    productTail(T, Acc * H).

% direct recursion of product
productDirect([]) ->
    1;
productDirect([H|T]) ->
    H * productDirect(T).

% tail recursion of max
maxTail([X|Xs]) ->
    maxTail(Xs, X).

maxTail([], Acc) -> 
    Acc;
maxTail([H|T], Acc) ->
    maxTail(T, max(Acc, H)).

% direct recursion of max
maxDirect([H]) ->
    H;
maxDirect([H|T]) ->
    max(H, maxDirect(T)).

%% Tests
% product tests
productTail_test() ->
    ?assertEqual(0, productTail([1, 2, 0, 4])),
    ?assertEqual(1, productTail([])),
    ?assertEqual(1, productTail([1])),
    ?assertEqual(120, productTail([1,2,3,5,4])).

productDirect_test() ->
    ?assertEqual(0, productDirect([1, 2, 0, 4])),
    ?assertEqual(1, productDirect([])),
    ?assertEqual(1, productDirect([1])),
    ?assertEqual(120, productDirect([1,2,3,5,4])).

% max tests
maxTail_test() ->
    ?assertError(function_clause, maxTail([])),
    ?assertEqual(1, maxTail([1])),
    ?assertEqual(5, maxTail([1,2,3,5,4])).

maxDirect_test() ->
    ?assertError(function_clause, maxDirect([])),
    ?assertEqual(1, maxDirect([1])),
    ?assertEqual(5, maxDirect([1,2,3,5,4])).