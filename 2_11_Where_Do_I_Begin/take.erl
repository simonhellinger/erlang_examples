-module(take).
-include_lib("eunit/include/eunit.hrl").

-export([take/2]).

-spec take(integer(), [T]) -> [T].

take(0, _) -> 
    [];
take(_, []) ->
    [];
take(N, [H|T]) ->
    [H | take(N - 1, T)].

take_test() ->
    ?assertEqual([], take(0, "hello")),
    ?assertEqual("hell", take(4, "hello")),
    ?assertEqual("hello", take(5, "hello")),
    ?assertEqual("hello", take(9, "hello")).