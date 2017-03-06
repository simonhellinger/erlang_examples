-module(rps).
-include_lib("eunit/include/eunit.hrl").

beat(Choice) ->
    case Choice of
        rock ->
            paper;
        paper ->
            scissors;
        scissors ->
            rock
    end.

lose(Choice) ->
    case Choice of
        paper ->
            rock;
        scissors ->
            paper;
        rock ->
            scissors
    end.

result(ChoiceA, ChoiceA) ->
    0;
result(ChoiceA, ChoiceB) ->
    case beat(ChoiceA) of
        ChoiceB ->
            -1;
        _ ->
            1
    end.

tournament(Lefts, Rights) ->
    Results = lists:zipwith(fun (L, R) -> result(L, R) end, Lefts, Rights),
    lists:foldr(fun (X, Sum) -> X + Sum end, 0, Results).


tournament_test() ->
    ?assertEqual(3, tournament([paper, scissors, paper], [rock, paper, rock])),
    ?assertEqual(0, tournament([paper, scissors, paper, scissors], [scissors, rock, rock, paper])),
    ?assertEqual(-1, tournament([paper, scissors, paper, paper], [scissors, rock, rock, paper])).


    
