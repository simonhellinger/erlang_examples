-module(shapes).
-include_lib("eunit/include/eunit.hrl").

-export([perimeter/1, enclose/1, area/1]).

%% Representations for supported shapes:
%
% Rectangle: {rectangle, {W, H}}
% Circle: {circle, R}
% Triangle: {triangle, {A, B, C}}
%
% Adhering to erlang's let-it-fail strategy, all other shapes cause an error.
%
%
% For further information see the file readme.md in the assignment directory. 

%% Helpers for readability
square(N) ->
    N * N.

maxThree({M, N, O}) ->
    max(max(M, N), O).

%% Perimeter
% Rectangle
perimeter({rectangle, {W, H}}) ->
    2 * W + 2 * H;

% Circle
perimeter({circle, R}) ->
    2 * R * math:pi();

% Triangle
perimeter({triangle, {A, B, C}}) ->
    A + B + C.



%% Area
% Rectangle
area({rectangle, {W, H}}) ->
    W * H;

% Circle
area({circle, R}) -> 
    square(R) * math:pi();

% Triangle
area({triangle, {A, B, C}}) ->
    % I'm using Heron's Area formula, as it needs only the sides A, B, C
    S = perimeter({triangle, {A, B, C}}) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).



%% Enclose
% Rectangle
enclose(N = {rectangle, _}) -> 
    N;

% Circle
enclose({circle, R}) ->
    D = 2 * R, 
    {rectangle, {D, D}};

% Triangle
enclose({triangle, SIDES}) ->
    BASE = maxThree(SIDES), % take the longest side
    HEIGHT = (2 * area({triangle, SIDES})) / BASE, % A = (BASE * HEIGHT) / 2 -> HEIGHT = (2 * A) / BASE
    {rectangle, {BASE, HEIGHT}}.



%% All tests sorted by shape, including one unsupported shape
rectangle_test() ->
    RECT = {rectangle, {4, 8}},
    ?assertEqual(24, perimeter(RECT)),
    ?assertEqual(32, area(RECT)),
    ?assertEqual(RECT, enclose(RECT)).

circle_test() ->
    CIRCLE = {circle, 5},
    ?assertEqual(10 * math:pi(), perimeter(CIRCLE)),
    ?assertEqual(25 * math:pi(), area(CIRCLE)),
    ?assertEqual({rectangle, {10, 10}}, enclose(CIRCLE)).

triangle_test() ->
    TRIANGLE = {triangle, {3, 4, 5}}, 
    ?assertEqual(12, perimeter(TRIANGLE)),
    ?assertEqual(6.0, area(TRIANGLE)),
    ?assertEqual({rectangle, {5, 2.4}}, enclose(TRIANGLE)).

polygon_test() ->
    POLYGON = {polygon, {2, 3, 4, 5, 6}},
    ?assertError(function_clause, perimeter(POLYGON)),
    ?assertError(function_clause, area(POLYGON)),
    ?assertError(function_clause, enclose(POLYGON)).