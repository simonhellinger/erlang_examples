-module(patterns).
-include_lib("eunit/include/eunit.hrl").
-export([x_orl1/2, x_orl2/2, x_or1/2, x_or2/2, x_or3/2, maxThree/3]).

% provided in the lesson
x_orl1(true, false) ->
    true;
x_orl1(false, true) ->
    true;
x_orl1(_,_) ->
    false.

%provided in the lesson
x_orl2(X, X) ->
    false;
x_orl2(_,_) ->
    true.

% one
x_or1(X, Y) ->
    {X,Y} == {X, not X}.

% two
x_or2(X, Y) ->
    (X and not Y) or (Y and not X).

% three
x_or3(X, Y) ->
    X =/= Y.


% maxThree
maxThree(X, X, X)->
    X;
maxThree(X, Y, Z)->
    max(max(X, Y), Z).

% tests
x_orl1_test() ->
    ?assertEqual(true, x_orl1(true,false)),
    ?assertEqual(true, x_orl1(false,true)),
    ?assertEqual(false, x_orl1(false,false)),
    ?assertEqual(false, x_orl1(true,true)).

x_orl2_test() ->
    ?assertEqual(true, x_orl2(true,false)),
    ?assertEqual(true, x_orl2(false,true)),
    ?assertEqual(false, x_orl2(false,false)),
    ?assertEqual(false, x_orl2(true,true)).

x_or1_test() ->
    ?assertEqual(true, x_or1(true,false)),
    ?assertEqual(true, x_or1(false,true)),
    ?assertEqual(false, x_or1(false,false)),
    ?assertEqual(false, x_or1(true,true)).

x_or2_test() ->
    ?assertEqual(true, x_or2(true,false)),
    ?assertEqual(true, x_or2(false,true)),
    ?assertEqual(false, x_or2(false,false)),
    ?assertEqual(false, x_or2(true,true)).

x_or3_test() ->
    ?assertEqual(true, x_or3(true,false)),
    ?assertEqual(true, x_or3(false,true)),
    ?assertEqual(false, x_or3(false,false)),
    ?assertEqual(false, x_or3(true,true)).

maxThree_test() ->
    ?assertEqual(5, maxThree(3,4,5)),
    ?assertEqual(5, maxThree(5,4,3)),
    ?assertEqual(5, maxThree(3,5,4)),
    ?assertEqual(0, maxThree(0,0,0)),
    ?assertEqual(1, maxThree(1,1,1)),
    ?assertEqual(1, maxThree(1,1,0)).