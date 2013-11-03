bounded-intmap
==============

`bounded-intmap` is a reimplementation of `Data.IntMap` that uses minimum and maximum bounds on subtrees instread of bit prefixes. The original idea, by Edward Kmett, is described [here](https://www.fpcomplete.com/user/edwardk/revisiting-matrix-multiplication/part-4). As per my current benchmark results, this implemenation seems to be 1-1.5x slower than stock `Data.IntMap`.

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

Description of the internals
----------------------------
### Figuring out which way to go ###
Suppose we are looking up a key `k` in a tree. We know that the minimum key in the tree is `min` and that the maximum key is `max`. Represented in binary:

             shared prefix   bit to split on
               /----------\  /
    min:       010010010101 0 ????????
    max:       010010010101 1 ????????
    k:         010010010101 ? ????????

To figure out in which subtree we need to recursively search for `k`, we need to know whether the bit to split on is zero or one. Now, if it is zero, then

    xor min k: 000000000000 0 ????????
    xor k max: 000000000000 1 ????????

If it is one:

    xor min k: 000000000000 1 ????????
    xor k max: 000000000000 0 ????????

Therefore, the splitting bit is set iff `xor min k > xor k max`. Taking the terminology from the original article, `insideR k min max = xor min k > xor k max`.
