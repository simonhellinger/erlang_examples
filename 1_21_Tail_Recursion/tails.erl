-module(tails).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1]).

% Fibonacci numbers
% 0, 1, 1, 2, 3, 5, 8, 13, 21


fib(N) ->
    fib(N, 0, 1).

fib(0, ACC, _NUM) ->
    ACC;
fib(N, ACC, NUM) ->
    fib(N - 1, ACC + NUM, ACC).

% fib(4) -> 
% fib(4, 0, 1)
% fib(3, 1, 0)
% fib(2, 1, 1)
% fib(1, 2, 1)
% fib(0, 3, 2)
% 5


fib_test() ->
    ?assertEqual(0, fib(0)),
    ?assertEqual(1, fib(1)),
    ?assertEqual(3, fib(4)),
    ?assertEqual(5, fib(5)),
    ?assertEqual(8, fib(6)).

% Perfect numbers


% Given a function F from int to int, find the sum of the values F(0), F(1), ..., F(N-1), F(N)
% Given a function F from int to int, find the max of the values F(0), F(1), ..., F(N-1), F(N)