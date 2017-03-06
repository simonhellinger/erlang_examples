-module(hofs).
-include_lib("eunit/include/eunit.hrl").

-export([double_all/1]).


double_all(Xs) -> lists:map(fun (X) -> X * 2 end, Xs).


double_all_test()->
    ?assertEqual([], double_all([])),
    ?assertEqual([0], double_all([0])),
    ?assertEqual([2, 4, 6], double_all([1, 2, 3])).