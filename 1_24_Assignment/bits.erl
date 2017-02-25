-module(bits).
-include_lib("eunit/include/eunit.hrl").
-export([bitSum/1, bitSumTail/1]).

%% The regular recursion
bitSum(0) ->
    0;
bitSum(N) when N >= 0 -> 
    N rem 2 + bitSum(N div 2).


%% Tail recursion version
bitSumTail(N) when N >= 0 ->
    bitSumTail(N, 0).

bitSumTail(0, ACC) ->
    ACC;
bitSumTail(N, ACC) ->
    bitSumTail(N div 2, ACC + (N rem 2)).


%% Tests
bitSum_test() ->
    ?assertError(function_clause, bitSum(-1)),
    ?assertEqual(0, bitSum(0)),
    ?assertEqual(1, bitSum(1)),
    ?assertEqual(1, bitSum(2)),
    ?assertEqual(2, bitSum(3)),
    ?assertEqual(7, bitSum(375)).

bitSumTail_test() ->
    ?assertError(function_clause, bitSumTail(-1)),
    ?assertEqual(0, bitSumTail(0)),
    ?assertEqual(1, bitSumTail(1)),
    ?assertEqual(1, bitSumTail(2)),
    ?assertEqual(2, bitSumTail(3)),
    ?assertEqual(7, bitSumTail(375)).