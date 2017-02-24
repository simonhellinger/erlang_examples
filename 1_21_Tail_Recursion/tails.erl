-module(tails).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1, perfN/1, sumOfResults/2, maxOfResults/2]).

% Fibonacci numbers
% 0, 1, 1, 2, 3, 5, 8, 13, 21

% for a better version see teacher_tail.erl
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
% 6
% 6 / 1 = 6
% 6 / 2 = 3
% 6 / 3 = 2
% 6 / 4 = x
% 6 / 5 = x
% 6 / 6 = 1
% 1 + 2 + 3 = 6

perfN(N) ->
    perfN(1, 0, N).

% perfN(counter, accumulator, original_value) -- for a better version see teacher_tail.erl
perfN(_, _, 0) ->
    false;
perfN(ORIG, ACC, ORIG) ->
    ACC == ORIG;
perfN(N, ACC, ORIG) when ORIG rem N == 0 ->
    perfN(N + 1, ACC + N, ORIG);
perfN(N, ACC, ORIG) when ORIG rem N /= 0 -> % guard unneccessary here, see teacher_tail.erl
    perfN(N + 1, ACC , ORIG).


perfN_test() ->
    ?assertEqual(false, perfN(0)),
    ?assertEqual(false, perfN(1)),
    ?assertEqual(false, perfN(2)),
    ?assertEqual(false, perfN(3)),
    ?assertEqual(true, perfN(6)),
    ?assertEqual(false,perfN(8)),
    ?assertEqual(true, perfN(28)).


% Given a function F from int to int, find the sum of the values F(0), F(1), ..., F(N-1), F(N)

sumOfResults(FUNC, N) ->
    sumOfResults(N, FUNC, 0).

sumOfResults(-1, _, ACC) ->
    ACC;
sumOfResults(N, FUNC, ACC) ->
    sumOfResults(N - 1, FUNC, ACC + FUNC(N)).

sumOfResults_test() ->
    ?assertEqual(0, sumOfResults(fun (X) -> X * X end, 0)),
    ?assertEqual(1, sumOfResults(fun (X) -> X * X end, 1)),
    ?assertEqual(91, sumOfResults(fun (X) -> X * X end, 6)).

% Given a function F from int to int, find the max of the values F(0), F(1), ..., F(N-1), F(N)
maxOfResults(FUNC, N) ->
    maxOfResults(N, FUNC, 0).

maxOfResults(-1, _, ACC) ->
    ACC;
maxOfResults(N, FUNC, ACC) ->
    maxOfResults(N - 1, FUNC, max(ACC, FUNC(N))).

maxOfResults_test() ->
    ?assertEqual(0, maxOfResults(fun (X) -> X * X end, 0)),
    ?assertEqual(1, maxOfResults(fun (X) -> X * X end, 1)),
    ?assertEqual(36, maxOfResults(fun (X) -> X * X end, 6)).