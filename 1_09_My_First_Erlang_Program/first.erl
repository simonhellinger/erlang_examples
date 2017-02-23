-module(first).
-include_lib("eunit/include/eunit.hrl").
-export([double/1, mult/2, area/3, square/1, treble/1]).

mult(X, Y) ->
    X * Y.

double(X) ->
    mult(2, X).

area(A,B,C) ->
    S = (A + B + C)/2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

square(A) ->
    mult(A, A).

treble(A) ->
    mult(3, A).

mult_test() ->
    ?assertEqual(1, mult(1, 1)),
    ?assertEqual(2, mult(1, 2)),
    ?assertEqual(100, mult(10, 10)),
    ?assertEqual(0, mult(0, 50)).

