-module(palindrome).
-include_lib("eunit/include/eunit.hrl").

-export([palindrome/1]).

palindrome([]) ->
    true;
palindrome(Xs) -> 
    Sanitized = string:to_lower(only_alphas(Xs)),
    SanitizedRev = reverse(Sanitized),
    Sanitized == SanitizedRev.

only_alphas([])->
    [];
only_alphas([X|Xs]) when X >= $A, X =< $z ->
    [X|only_alphas(Xs)];
only_alphas([_X|Xs]) ->
    only_alphas(Xs).

reverse(Xs) ->
    reverse(Xs, []).

reverse([], Acc) ->
    Acc;
reverse([X|Xs], Acc) ->
    reverse(Xs, [X|Acc]).


reverse_test() ->
    ?assertEqual([], reverse([])),
    ?assertEqual([1], reverse([1])),
    ?assertEqual([2, 1], reverse([1, 2])),
    ?assertEqual([3, 2, 1], reverse([1, 2, 3])).

palindrome_test() ->
    ?assertEqual(true, palindrome([])),
    ?assertEqual(false, palindrome("hello World")),
    ?assertEqual(true, palindrome("Anna")),
    ?assertEqual(true, palindrome("Madam I'm Adam.")).