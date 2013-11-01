bounded-intmap
==============

`bounded-intmap` is a reimplementation of `Data.IntMap` that uses minimum and maximum bounds on subtrees instread of bit prefixes. The original idea, by Edward Kmett, is described [here](https://www.fpcomplete.com/user/edwardk/revisiting-matrix-multiplication/part-4). As per my [current benchmark results](https://github.com/gereeter/bounded-intmap/blob/master/benchmarks/report.html), this implemenation seems to be 1.4-4x faster than stock `Data.IntMap`.

I deviate from the original implementation in a couple of ways:

* I removed the redundant encoding of bounds. Previously, you might have a tree like this:

               0,7
              /    \
           0,3      4,7 
          /   \     /  \
        0,1   2,3  4,5  6,7

  Now, you have trees like this:

          0,7
          /  \
         3    4 
        / \  / \
       1   2 5  6

  Note that this means that this implementation consumes less memory than the current `Data.IntMap`.

* I factored the datatype into two pieces: `Node` for non-empty trees, and `WordMap` for possibly empty trees.
* I added an optimization that seems to have helped immensely - I cache some of the computation for locating a key as I traverse the key, making it quicker to decide which way to go.
