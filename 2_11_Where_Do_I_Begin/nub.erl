-module(nub).
-include_lib("eunit/include/eunit.hrl").

-export([nub/1]).


nub([]) ->
    [];
nub([X|Xs]) ->
    [X|nub(remove_all(X, Xs))].

remove_all(_Elem, []) ->
    [];
remove_all(Elem, [X|Xs]) ->
    case X == Elem of
        true ->
            remove_all(Elem, Xs);
        false ->
            [X|remove_all(Elem,Xs)]
    end.

nub_test() ->
    ?assertEqual([], nub([])),
    ?assertEqual([1], nub([1])),
    ?assertEqual([1], nub([1, 1])),
    ?assertEqual([1, 2, 3], nub([1, 1, 2, 3, 2, 3])).