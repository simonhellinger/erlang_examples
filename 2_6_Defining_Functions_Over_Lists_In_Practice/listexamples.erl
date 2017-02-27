-module(listexamples).
-include_lib("eunit/include/eunit.hrl").

-export([productTail/1, productDirect/1]).

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

%% product tests
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

%max(Xs) -> Xs.
%
%max_test() ->
%    ?assertError(function_call, productTail([])),
%    ?assertEqual(1, productTail([1])),
%    ?assertEqual(5, productTail([1,2,3,5,4])).