-module(reclists).
-include_lib("eunit/include/eunit.hrl").

-export([double/1, evens/1]).

%% Double is a map operation
%% using direct recursion for now
double([]) -> 
    [];
double([H | T]) ->
    [2 * H | double(T)].

%% evens is a filter operation
%% using direct recursion for now
evens([]) ->
    [];
evens([H | T]) ->
    case H rem 2 of
        0 ->
            [H | evens(T)];
        _ ->
            evens(T)
    end.

double_test() ->
    ?assertEqual([], double([])),
    ?assertEqual([0], double([0])),
    ?assertEqual([2], double([1])),
    ?assertEqual([2, 4, 6, 8, 10], double([1, 2, 3, 4, 5])).

evens_test() ->
    ?assertEqual([], evens([])),
    ?assertEqual([0], evens([0])),
    ?assertEqual([], evens([1])),
    ?assertEqual([2, 4], evens([1, 2, 3, 4, 5])).