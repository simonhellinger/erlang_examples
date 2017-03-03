-module(consolidating).
-include_lib("eunit/include/eunit.hrl").

-export([join/2, concat/1, merge_sort/1, quick_sort/1, insertion_sort/1]).

%%
%% joins two lists (and, thus, strings) together.
%%
join(Xs, []) ->
    Xs;
join(Xs, Ys) ->
    reverse(prepend(reverse(Xs), Ys)).

% Prepends first argument list to second arguments list, symbol by symbol.
% So prepend ("abc", "def") becomes "cdadef".
prepend(Xs, []) ->
    Xs;
prepend(Xs, [Y | Ys]) ->
    prepend([Y|Xs], Ys).

% Turns a list around
reverse(Xs) ->
    reverse(Xs, []).

reverse([], Acc) ->
    Acc;
reverse([X | Xs], Acc) ->
    reverse(Xs, [X | Acc]).

join_test() ->
    ?assertEqual([], join([], [])),
    ?assertEqual("a", join([], "a")),
    ?assertEqual([1], join([1], [])),
    ?assertEqual("ab", join("a", "b")),
    ?assertEqual("Hello World", join("Hello", " World")).

%%
%% Concatenation of "unlimited"" lists into one list.
%%
concat(Xs) ->
    concat(Xs, []).

concat([], Acc) ->
    Acc;
concat([X | Xs], Acc) ->
    concat(Xs, join(Acc, X)).

concat_test() ->
    ?assertEqual([], concat([])),
    ?assertEqual([1, 2, 3, 4], concat([[1], [2, 3], [4]])),
    ?assertEqual("goodbye, world", concat(["goo", "dbye,", " ", "world"])).

%%
%% Checks whether an element is in a list
%%
member(_, []) ->
    false;
member(N, [X|Xs]) ->
    case N of
        X -> 
            true;
        _ ->
            member(N, Xs)
    end.

member_test() ->
    ?assertEqual(false, member(1, [])),
    ?assertEqual(true, member(1, [1])),
    ?assertEqual(false, member(2, [1])),
    ?assertEqual(true, member(2, [1, 3, 4, 2, 6])),
    ?assertEqual(true, member([3, 4], [1, 2, [3, 4], 5])).

%%
%% Sorting algorightm helpers
%%

% Returns the length of a list
len(L) ->
    len(L, 0).

len([], Acc) ->
    Acc;
len([_X | Xs], Acc) ->
    len(Xs, Acc + 1).

% Splits a list roughly in half
split(L) ->
    split(reverse(L), [], len(L) div 2).

split([], R, _) ->
    R;
split(L, R, 0) ->
    [L, R];
split([L|Ls], Right, Elems) ->
    split(Ls, [L|Right], Elems - 1).

%%
%% Merge sort
%%

%% The actual sorting happens in merge.
merge_sort([]) ->
    [];
merge_sort([N]) ->
    [N];
merge_sort(L) ->
    [Left, Right] = split(L),
    merge(merge_sort(Left), merge_sort(Right)).

% This is the workhorse of merge_sort
merge(A, []) ->
    A;
merge([], B) ->
    B;
merge([A|As] = LA, [B|Bs] = LB) ->
    case A < B of
        true ->
            [A | merge(As, LB)];
        false ->
            [B | merge(LA, Bs)]
    end.

%% Quick sort
quick_sort([]) ->
    [];
quick_sort([X|_Xs] = L) ->
    {Left, Right} = partition(L, X),
    concat([quick_sort(Left), [X], quick_sort(Right)]).

% this is the workhorse of quick_sort
partition(L, P) ->
    partition(L, P, [], []).

partition([], _, Left, Right) ->
    {Left, Right};
partition([X|Xs], Pivot, Left, Right) when Pivot == X ->
    partition(Xs, Pivot, Left, Right);
partition([X|Xs], Pivot, Left, Right) ->
    case Pivot > X of
        true ->
            partition(Xs, Pivot, [X|Left], Right);
        false ->
            partition(Xs, Pivot, Left, [X|Right])
    end.


partition_test() ->
    ?assertEqual({[], []}, partition([], 0)),
    ?assertEqual({[], [1]}, partition([1], 0)),
    ?assertEqual({[], [2]}, partition([2, 1], 1)),
    ?assertEqual({[5, 2, 3],  [8, 9, 7]}, partition([3, 2, 5, 6, 7, 9, 8], 6)).


%%
%% Insertion sort
%%
insertion_sort([]) ->
    [];
insertion_sort([X|Xs]) ->
    insert(X, insertion_sort(Xs)).

insert(X, []) ->
    [X];
insert(X, [Y|Ys] = A) ->
    case X < Y of
        true ->
            [X| A];
        false ->
            [Y | insert(X, Ys)]
    end.

%%
%% Sorting tests
%%
empty_list_test() ->
    Act = [],
    Exp = [],
    ?assertEqual(Exp, merge_sort(Act)),
    ?assertEqual(Exp, quick_sort(Act)),
    ?assertEqual(Exp, insertion_sort(Act)).

single_element_list_test() ->
    Act = [1],
    Exp = [1],
    ?assertEqual(Exp, merge_sort(Act)),
    ?assertEqual(Exp, quick_sort(Act)),
    ?assertEqual(Exp, insertion_sort(Act)).

two_element_list_test() ->
    Act = [2, 1],
    Exp = [1, 2],
    ?assertEqual(Exp, merge_sort(Act)),
    ?assertEqual(Exp, quick_sort(Act)),
    ?assertEqual(Exp, insertion_sort(Act)).

regular_list_test() ->
    Act = [900, 1, 70, 4, 2, 3, 5, 6, 7],
    Exp = [1, 2, 3, 4, 5, 6, 7, 70, 900],
    ?assertEqual(Exp, merge_sort(Act)),
    ?assertEqual(Exp, quick_sort(Act)),
    ?assertEqual(Exp, insertion_sort(Act)).