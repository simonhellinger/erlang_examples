-module(far).
-include_lib("eunit/include/eunit.hrl").

-export([add/1,times/1,compose/2,id/1,iterate/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

iterate(0) -> 
    fun id/1;
iterate(N) when N > 0 ->
    fun(Fn) ->
        lists:foldl(fun compose/2, fun id/1, lists:duplicate(N, Fn))
    end.



compose_fns(Fns) ->
    lists:foldr(fun far:compose/2 , fun far:id/1, Fns).


compose_fns_test() ->
    DoStuff = compose_fns([fun (X) -> X * 3 end, fun (X) -> X - 5 end, fun(X) -> X * 2 end]),
    ?assertEqual(20, DoStuff(5)).

twice(F) ->
    compose(F, F).

mult_by_3(X) ->
    X * 3.

twice_test() ->
    TwiceTimes3 = twice(fun mult_by_3/1),
    ?assertEqual(18, TwiceTimes3(2)),
    TwiceTwice = twice(fun twice/1),
    TwiceTwiceTimes3 = TwiceTwice(fun mult_by_3/1),
    ?assertEqual(1, TwiceTwiceTimes3(2)).
