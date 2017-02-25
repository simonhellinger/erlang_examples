-module(assignment).
-include_lib("eunit/include/eunit.hrl"). % the unit testing library

-export([perimeter/1, enclose/1, area/1]).

%% Helper functions for the sake of readability
% squares a number
square(N) ->
    N * N.

%% Calculates the perimeter for various shapes.
%% Supports circles, rectangles, squares and triangles
perimeter({circle, R}) ->
    2 * R * math:pi();
perimeter({rectangle, {W, H}}) ->
    2 * W + 2 * H;
perimeter({square, S}) ->
    perimeter({rectangle, {S, S}}); % use what we already have
perimeter({triangle, {A, B, C}}) ->
    A + B + C;
perimeter(_) -> % All other cases return undefined
    undefined.

%% Calculates the area for various shapes.
%% Supports circles, rectangles, squares and triangles
area({circle, R}) -> 
    square(R) * math:pi();
area({rectangle, {W, H}}) ->
    W * H;
area({square, S}) ->
    area({rectangle, {S, S}}); % reusing function might be inefficient...
area({triangle, {A, B, C}}) ->
    % We're using Heron's Area formula
    S = perimeter({triangle, {A, B, C}}) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C));
area(_) ->
    undefined.

%% Calculates the smallest enclosing rectangle for various shapes
%% Supports circles, rectangles, squares, but not triangles 
%% (the assignment does not ask for triangles, and they seem to be the most difficult part)
enclose({circle, R}) ->
    D = 2 * R, 
    {rectangle, {D, D}};
enclose(N = {rectangle, _}) -> 
    N;
enclose({square, S}) ->
    enclose({rectangle, {S, S}});
enclose(_) ->
    undefined.

perimeter_test() ->
    ?assertEqual(12, perimeter({triangle, {3, 4, 5}})),
    ?assertEqual(32, perimeter({square, 8})),
    ?assertEqual(10 * math:pi(), perimeter({circle, 5})),
    ?assertEqual(undefined, perimeter({polygon, {2, 3, 4, 5, 6}})).

area_test() ->
    ?assertEqual(6.0, area({triangle, {3, 4, 5}})),
    ?assertEqual(64, area({square, 8})),
    ?assertEqual(25 * math:pi(), area({circle, 5})),
    ?assertEqual(undefined, perimeter({polygon, {2, 3, 4, 5, 6}})).

enclose_test() ->
    ?assertEqual({rectangle, {8, 9}}, enclose({rectangle, {8, 9}})),
    ?assertEqual({rectangle, {8, 8}}, enclose({square, 8})),
    ?assertEqual({rectangle, {10, 10}}, enclose({circle, 5})),
    ?assertEqual(undefined, enclose({polygon, {2, 3, 4, 5, 6}})).