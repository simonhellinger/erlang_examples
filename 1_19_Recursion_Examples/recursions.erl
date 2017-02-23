-module(recursions).
-include_lib("eunit/include/eunit.hrl").
-export([fib/1, cuts/1]).

% fibonacci numbers
% 0 1 1 2 3 5 8 13 21 ...

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N > 1 ->
    fib(N - 1) + fib(N - 2).


fib_test() ->
    ?assertEqual(0, fib(0)),
    ?assertEqual(1, fib(1)),
    ?assertEqual(3, fib(4)),
    ?assertEqual(5, fib(5)),
    ?assertEqual(8, fib(6)).


% fib(4) ->
% fib(2) + fib(3)
% fib(0) + fib(1) + fib(1) + fib(2)
% fib(0) + fib(1) + fib(1) + fib(0) + fib(1)
% 0 + 1 + 1 + 0 + 1
% 3


% lazy caterer's sequence ('how many cuts')
% 0 cuts ... 1 piece
% 1 ........ 2
% 2 ........ 4
% 3 ........ 7
% 4 ....... 11
% 5 ....... 16
% 6 ....... 22
% ...

cuts(0) ->
    1;
cuts(N) when N >= 0 ->
    cuts(N - 1) + N.

% cuts(4) ->
% cuts(3) + 4
% cuts(2) + 3 + 4
% cuts(1) + 2 + 3 + 4
% cuts(0) + 1 + 2 + 3 + 4
% 1 + 1 + 2 + 3 + 4
% 11

cuts_test() ->
    ?assertEqual(1, cuts(0)),
    ?assertEqual(2, cuts(1)),
    ?assertEqual(4, cuts(2)),
    ?assertEqual(7, cuts(3)),
    ?assertEqual(11, cuts(4)),
    ?assertEqual(16, cuts(5)),
    ?assertEqual(22, cuts(6)).

% alternate, non-recursive way
cutsNr(N) ->
    (N * N + N + 2) div 2.

cutsNr_test() ->
    ?assertEqual(1, cutsNr(0)),
    ?assertEqual(2, cutsNr(1)),
    ?assertEqual(4, cutsNr(2)),
    ?assertEqual(7, cutsNr(3)),
    ?assertEqual(11, cutsNr(4)),
    ?assertEqual(16, cutsNr(5)),
    ?assertEqual(22, cutsNr(6)).

% In 3d the same problem is called the cake number:
% 0 cuts .... 1 piece
% 1 ......... 2
% 2 ......... 4
% 3 ......... 8
% 4 ........ 15
% 5 ........ 26
% 6 ........ 42
% ...