-module(second).
-export([hypotenuse/2, area/2, perimeter/2]).

hypotenuse(A, B) ->
    math:sqrt(first:square(A) + first:square(B)).

area(A, B) ->
    first:area(A, B, hypotenuse(A, B)).

perimeter(A, B) ->
    A + B + hypotenuse(A, B).