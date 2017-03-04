-module(index).
-include_lib("eunit/include/eunit.hrl").
-export([get_file_contents/1, show_file_contents/1, index_text/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    


% Notes:
%
% There is a readme.md with some interesting information, most of it can be found here as well.
%
% If a certain functionality like sorting, reversing or lower-casing has been
% the topic of an earlier exercise, I used library functions instead. I see no
% use in doing these all over again.
%
% There are several tests at the bottom of this file. To run them, call
%
% c(index).
% eunit:test(index).
%
% To run the example in the command line, call index:index_text(index:get_file_contents("gettysburg-address.txt")).


% The starting point of the application; this indexes
% the whole text line by line.
%
% The index returned ignores case and is ordered lexicographically
% using lists:sort.

index_text(Lines) ->
    Index = index_lines(Lines),
    SortedIndex = lists:sort(Index), % Sorts tuples first element to last
    group_line_numbers(SortedIndex).

% Here we populate the index line by line.
% 
% 1) First get rid of punctuation and case sensitivity on the whole line.
% 2) Then we split the line into separate words
% 3) Eliminate stopwords (common, irrelevant words)
% 4) After that we populate the index with each word of the line (in a separate function).

index_lines(Lines) ->
    index_lines(Lines, 1, []).

index_lines([], _, Index) ->
    Index;
index_lines([L|Ls], LineNumber, Index) ->
    Words = eliminate_stop_words(split_line(string:to_lower(L))),
    index_lines(Ls, LineNumber + 1, index_words(LineNumber, Words, Index)).

% Here we split a line into a list of words, losing all punctuation on the way.
% We use a tail recursive function that holds one buffer (Word) for building the current word from letters
% and a second buffer (Words) that holds the already found Word-buffers.

split_line(Line) ->
    split_line(Line, [], []).

split_line([], [], Words) ->
    Words;
split_line([], Word, Words) ->
    [lists:reverse(Word) | Words];
split_line([L | Ls], Word, Words) when L >= $A, L =< $z -> % put next letter in word
    split_line(Ls, [L | Word], Words);
split_line([_L | Ls], Word, Words) -> % put next word on word-list, as soon as non-letter symbol is found
    case Word of
        [] ->
            split_line(Ls, [], Words);
        _ ->
            split_line(Ls, [], [lists:reverse(Word)|Words])
    end.

% Starting with a list of already lowercased words and no punctuation, we
% proceed to put each word in the index.

index_words(_, [], Index) ->
    Index;
index_words(LineNumber, [W | Ws], Index) ->
    index_words(LineNumber, Ws, update_index(Index, W, LineNumber)).

% We update the index by looking for the word we want to update.
% If we find it, we update the list of line-numbers we found it in.
% If we don't find it, we add a new entry to the list.
%
% The order of the resulting list is of no relevance, as the list is sorted
% once in the end.

update_index([], Word, Line) ->
    [{Word, [Line]}];
update_index([{Word, Lines} | Is], Word, Line) ->
    [{Word, insert_num_if_not_exists(Line, Lines)} | Is];
update_index([I| Is], Word, Line) ->
    [I | update_index(Is, Word, Line)].

% We insert a number into a list of numbers. We put it in the right place
% so we don't need to sort it later. And we're ignoring duplicates, as well,
% one mention of the line is enough.
insert_num_if_not_exists(N, []) ->
    [N];
insert_num_if_not_exists(L, [L|_Ls] = LineNumbers) -> % already exists, we're done
    LineNumbers;
insert_num_if_not_exists(N, [L|Ls] = LineNumbers) ->
    case N > L of
        true ->
            [L | insert_num_if_not_exists(N, Ls)];
        false ->
            [N | LineNumbers]
    end.

% We use this function to go over all elements of the Index
% and group the line numbers according to the
% assignment's demands.
%
% In the big picture, this is done on the finished index,
% as doing it any sooner would likely result in a wrongly filled index list.

group_line_numbers([]) ->
    [];
group_line_numbers([{Word, LineNumbers}| Words]) ->
    Grouped = {Word, group_numbers(LineNumbers)}, 
    [Grouped | group_line_numbers(Words)].
    
% We bunch a list of sorted numbers into
% groups. Each group holds only consecutive numbers, as soon as there's a
% gap, a new group is started. Single, non-consecutive lines are represented
% as {7, 7} for example. 

group_numbers([]) ->
    [];
group_numbers([N|Ns]) ->
    lists:reverse(group_numbers(Ns, {N,N}, [])).

group_numbers([], Group, Groups) ->
    [Group|Groups];
group_numbers([N|Ns], {S, E}, Groups) when E + 1 == N ->
    group_numbers(Ns, {S, N}, Groups);
group_numbers([N|Ns], {S, E}, Groups) ->
    group_numbers(Ns, {N, N}, [{S, E}| Groups]).

% removes stop words (a list of most irrelevant english words). The stop words
% are stored in an extra file, one word per line, which makes the file loading
% mechanism the perfect helper.
%
% We ignore any word sorting, as this will be done once in the end.

eliminate_stop_words(Words) ->
    StopWords = load_stop_words(),
    eliminate_stop_words(Words, StopWords, []).

eliminate_stop_words([], _, Result) ->
    Result;
eliminate_stop_words([W|Ws], StopWords, Result)->
    case lists:member(W, StopWords) of
        true ->
            eliminate_stop_words(Ws, StopWords, Result);
        false ->
            eliminate_stop_words(Ws, StopWords, [W|Result])
    end.

load_stop_words() ->
    get_file_contents("stopwords.txt").

% The tests
%
% I wrote tests for almost every step of the way to see that
% I was on the right track. That is rather bad practice: Usually, 
% only the exported functions should be tested, as they cover 
% the rest of the functions completely and provide documentation 
% as to how to use the program.

index_text_test() ->
    ?assertEqual([{"bother", [{4, 4}, {6, 6}]}, {"text", [{1, 3}, {5, 5}, {7, 8}]}], 
                index_text(["Text", "text text", "text", "bother", "text", "bother", "text", "text"])),
    ?assertEqual([{"explanation", [{1, 1}]}, {"long", [{1, 3}]}, {"lost", [{3, 3}]}, {"sentence", [{2, 2}]}, {"winded", [{1, 1}]}], 
                index_text(["The long winded explanation", "A long sentence...", "Long lost!"])).

index_lines_test() ->
    ?assertEqual([{"hello", [1]}, {"dear", [1]}, {"oh", [2]}], index_lines(["Hello, my dear", "Oh My..."])).

index_line_test() ->
    ?assertEqual([{"hello", [13]}, {"dear", [13]}, {"World", [13]}], index_words(13, ["hello", "dear", "World"], [])),
    ?assertEqual([{"World", [1, 13]}, {"hello", [13]}, {"dear", [13]}], index_words(13, ["hello", "dear", "World"], [{"World", [1]}])).

split_line_test() ->
    ?assertEqual([], split_line([])),
    ?assertEqual(["hello"], split_line("hello")),
    ?assertEqual(["World", "hello"], split_line("hello, World")),
    ?assertEqual(["World", "hello"], split_line("hello, World!")).

update_index_test() ->
    ?assertEqual([{"hello", [1]}], update_index([], "hello", 1)),
    ?assertEqual([{"bye", [1]}, {"hello", [1]}], update_index([{"bye", [1]}], "hello", 1)),
    ?assertEqual([{"hello", [1, 10]}], update_index([{"hello", [1]}], "hello", 10)),
    ?assertEqual([{"bye", [1]}, {"hello", [1, 10]}], update_index([{"bye", [1]}, {"hello", [1]}], "hello", 10)).

group_numbers_test() ->
    ?assertEqual([], group_numbers([])),
    ?assertEqual([{1,3}, {5, 5}, {7, 10}], group_numbers([1, 2, 3, 5, 7, 8, 9, 10])).

insert_num_if_not_exists_test() ->
    ?assertEqual([0], insert_num_if_not_exists(0, [])),
    ?assertEqual([0, 1], insert_num_if_not_exists(0, [1])),
    ?assertEqual([0, 1, 2], insert_num_if_not_exists(2, [0, 1])),
    ?assertEqual([0, 1, 2], insert_num_if_not_exists(0, [1, 2])),
    ?assertEqual([30, 40, 50], insert_num_if_not_exists(40, [30, 50])).

eliminate_stop_words_test() ->
    ?assertEqual([], eliminate_stop_words([])),
    ?assertEqual([], eliminate_stop_words(["the"])),
    ?assertEqual(["afoot", "game"], eliminate_stop_words(["the", "game", "is", "afoot"])).