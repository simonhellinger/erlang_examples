# Indexing a file by line

## The task

The assignment is to index a file's words by the line numbers the words occurred in. The line numbers are grouped in ranges, as can be seen by the example given in the assignment's text:

```erlang
{ "foo" , [{3,5},{7,7},{11,13}] }
```

## Running the tests and the program

The application is stored in the file ``index.erl```, along with a bunch of eunit tests. To compile and run the tests, download the file - along with all the text files - to your computer and run:

```erl
c(index).
eunit:test(index).
```

To actually run the program, type:

```erl
index:index_text(index:get_file_contents("gettysburg-address.txt")).
```

## Optional tasks done

I tried to fulfill the following optional challenges:

* Remove all common words (stop-words, more on that later)

* Sort the output index in lexicographical order

* Normalize all words to lower case

## Optional tasks skipped

I did not touch the following optional challenges:

* Remove all short words. (I already remove stop words, which, in my opinion, makes more sense)

* Normalising towards common endings, plurals, &c. (To try and really make this grammar-proof takes considerable effort)

## Optional tasks thought about at length:

### Think on how you could make the data structure more efficient

The only answer I could readily come up with is to split the words in letters and build a search tree from it. These trees are called 'trie' and are a read optimization. Tries hold each letter of a word in a node, and following the nodes will lead you to the full word. For more information see [this wikipedia article about tries](https://en.wikipedia.org/wiki/Trie)

### Can you think of other ways that you might extend your solution?

One thing would be to make the normalization more unicode-friendly. Right now my solution breaks on words with special punctuation like diaresis (Emily Brontë) or accents (blasé). So one expansion would be to filter not on letters between $A and $z, but a more sophisticated way of actually finding which symbols are part of the word and which trigger the end of a word.

## Notes on my solution

### Use of libraries

Whenever useful I introduced erlang libraries such as ``lists`` or ``string`` to do things such as sorting, reversing, and finding membership in a list. I did this mainly where the problem at hand was already topic of a previous assignment or activity, so I just didn't want to reinvent the wheel again. Also, I think, a good knowledge of the available libraries makes you a much more efficient programmer, so there.

### Removal of all common words

I took a list of stop-words (a list of words that are usually irrelevant in a search) and used it to pare down the indexable text to it's necessary minimum. The file holding the stop-words holds one String per line, so I could reuse the given loading mechanism to load the stop-words into the program. So right now, if you want to to or remove from the list of stop-words, all you have to do is edit the file.

The  file itself was taken from [this page](http://xpo6.com/list-of-english-stop-words/). 