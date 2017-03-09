-module(rps).
-include_lib("eunit/include/eunit.hrl").
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,beat_least_frequent/1,beat_most_frequent/1,random_strategy/1,val/1,tournament/2]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

loses(rock) ->
    paper;
loses(paper) ->
    scissors;
loses(scissors) ->
    rock.

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.



% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS
% Function parameter is always the opponents list of plays

% Play what would've beaten by opponent's latest play, won't beat it now
% as opponent won't repeat.
no_repeat([]) ->
    paper;
no_repeat([X|_]) -> % opponent does not repeat
    beats(X).       % take the one she would've beat now, so she won't beat it next time

const(Play) ->
    dummy.

cycle(Xs) ->
    enum(length(Xs) rem 3).

% Choose next play randomly.

rand(_) ->
    enum(rand:uniform(3) - 1).

% Uses the element played most by the opponent, assuming
% it will play it more than the others.
beat_most_frequent(Xs) -> 
    {R, P, S} = bin(Xs),
    MstFrq = max(max(R, P), S),
    OpponentsPlay = 
        case MstFrq of
            R -> rock;
            P -> paper;
            S -> scissors
        end,
    loses(OpponentsPlay).

% Uses the element played least by the opponent, assuming the opponent
% will play all elements equally in the long run.
beat_least_frequent(Xs) -> 
    {R, P, S} = bin(Xs),
    LstFrq = min(min(R, P), S),
    OpponentsPlay = 
        case LstFrq of
            R -> rock;
            P -> paper;
            S -> scissors
        end,
    loses(OpponentsPlay).

% Selects each played hand into one of the bins {R, P, S}.
bin(Xs) ->
    lists:foldr(fun bin/2, {0, 0, 0}, Xs).

bin(rock, {R, P, S}) ->
    {R + 1, P, S};
bin(paper, {R, P, S}) ->
    {R, P + 1, S};
bin(scissors, {R, P, S}) ->
    {R, P, S + 1}.


random_strategy(Strategies) ->
    N = rand:uniform(length(Strategies)),
    lists:nth(N, Strategies).

% Strategy tests

cycle_test() -> % cycle depends on the length of the list
    ?assertEqual(rock, cycle([])),
    ?assertEqual(paper, cycle([rock, rock, paper, scissors])),
    ?assertEqual(scissors, cycle([rock, rock, paper, scissors, rock])),
    ?assertEqual(rock, cycle([rock, rock, paper, scissors, rock, scissors])).

no_repeat_test() ->
    ?assertEqual(paper, no_repeat([])),
    ?assertEqual(rock, no_repeat([paper])).

beat_most_frequent_test() ->
    ?assertEqual(paper, beat_most_frequent([])),
    ?assertEqual(paper, beat_most_frequent([paper, rock, paper, rock, scissors, rock])),
    ?assertEqual(scissors, beat_most_frequent([paper, rock, paper, rock, paper, paper])),
    ?assertEqual(rock, beat_most_frequent([scissors, rock, scissors, rock, scissors, scissors])).

beat_least_frequent_test() ->
    ?assertEqual(paper, beat_least_frequent([])),
    ?assertEqual(rock, beat_least_frequent([paper, rock, paper, rock, scissors, rock])),
    ?assertEqual(rock, beat_least_frequent([paper, rock, paper, rock, paper, paper])),
    ?assertEqual(scissors, beat_least_frequent([scissors, rock, scissors, rock, scissors, scissors])).