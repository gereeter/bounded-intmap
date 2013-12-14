bounded-intmap
==============

`bounded-intmap` is a reimplementation of `Data.IntMap` that uses minimum and maximum bounds on subtrees instread of bit prefixes. The original idea, by Edward Kmett, is described [here](https://www.fpcomplete.com/user/edwardk/revisiting-matrix-multiplication/part-4). As per my current benchmark results, this implemenation seems to range from 33% faster to 25% slower than stock `Data.IntMap`. Note that only one function in the benchmark, `insert`, is slower than stock `Data.IntMap`, and even then only if the key is not already present in the map.

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
* I cache some of the computation for locating a key as I traverse the key, making it quicker to decide which way to go.

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

Current Progress
----------------
Below is a listing of every function in stock `Data.IntMap`, along with the implementation state in `bounded-intmap`. There are three implementation states:

* Raw means that I have implemented the function directly. These functions should be on par with or faster than their corresponding functions in stock `Data.IntMap`.
* Delegated means that I have implemented the function, but in terms of other functions. This usually means that it will be slower than stock `Data.IntMap`, sometimes asymptotically, and I haven't figured out how to implement it (or implement it nicely) yet. Note that some functions marked as such, like `insertWithKey`, are trivial uses of other functions are should have almost no performance hit.
* Unimplemented means that I have yet to implement the function in any form.

### Operators
* `(!)`. Delegated, using `findWithDefault`.
* `(\\)`. Delegated, using `difference`.

### Query
* `null`. Raw.
* `size`. Raw.
* `member`. Raw.
* `notMember`. Raw.
* `lookup`. Raw.
* `findWithDefault`. Raw.
* `lookupLT`. Raw.
* `lookupGT`. Raw.
* `lookupLE`. Raw.
* `lookupGE`. Raw.

### Construction
* `empty`. Raw.
* `singleton`. Raw.

#### Insertion
* `insert`. Raw.
* `insertWith`. Raw.
* `insertWithKey`. Delegated, using `insertWith`.
* `insertLookupWithKey`. Delegated, using `lookup` and `insertWithKey`.

#### Delete/Update
* `delete`. Raw.
* `adjust`. Raw.
* `adjustWithkey`. Delegated, using `adjust`.
* `update`. Raw.
* `updateWithKey`. Delegated, using `update`.
* `updateLookupWithKey`. Delegated, using `lookup` and `updateWithKey`.
* `alter`. Delegated, using `member` and either `update` or `insert`.

### Combine
#### Union
* `union`. Delegated, using `unionWith`.
* `unionWith`. Delegated, using `unionWithKey`.
* `unionWithKey`. Delegated, using `foldrWithKey` and lots of `insertWithKey`s.
* `unions`. Delegated, using lots of `union`s.
* `unionsWith`. Delegated, using lots of `unionWith`s.

#### Difference
* `difference`. Delegated, using `foldrWithKey'` and lots of `delete`s.
* `differenceWith`. Delegated, using `differenceWithKey`.
* `differenceWithKey`. Delegated, using `foldrWithKey'` and lots of `update`s.

#### Intersection
* `intersection`. Delegated, using `intersectionWith`.
* `intersectionWith`. Delegated, using `intersectionWithKey`.
* `intersectionWithKey`. Delegated, using `mapMaybeWithKey` and lots of `lookup`s.

#### Universal combining function
* `mergeWithKey`. _Unimplemented_. Probably never will be implemented, at least in its current form, due to this being very implementation-specific.

### Traversal
#### Map
* `map`. Raw. Actually, this is sort of delegated to `fmap`, but since the delegation is just `map = fmap` and will probably be inlined, I count this as raw.
* `mapWithKey`. Raw.
* `traverseWithKey`. Raw.
* `mapAccum`. Delegated, using `mapAccumWithKey`.
* `mapAccumWithKey`. Raw.
* `mapAccumRWithKey`. Raw.
* `mapKeys`. Delegated, using `foldrWithKey'` and lots of `insert`s.
* `mapKeysWith`. Delegated, using `foldrWithKey'` and lots of `insertWith`s.
* `mapKeysMonotonic`. Delegated, using `mapKeys`.

#### Folds
* `foldr`. Raw.
* `foldl`. Raw.
* `foldrWithKey`. Raw.
* `foldlWithKey`. Raw.
* `foldMapWithKey`. Raw.

#### Strict folds
* `foldr'`. Raw.
* `foldl'`. Raw.
* `foldrWithKey'`. Raw.
* `foldlWithKey'`. Raw.

### Conversion
* `elems`. _Unimplemented_.
* `keys`. _Unimplemented_.
* `assocs`. _Unimplemented_.
* `keysSet`. _Unimplemented_. Note that I'm not sure whether to convert to stock `Data.IntSet` or `Data.WordSet`, which is much more in flux than `Data.WordMap`.
* `fromSet`. _Unimplemented_. Note that I'm not sure whether to convert from stock `Data.IntSet` or `Data.WordSet`, which is much more in flux than `Data.WordMap`.

#### Lists
* `toList`. Raw.
* `fromList`. Delegated, using lots of `insert`s.
* `fromListWith`. _Unimplemented_.
* `fromListWithKey`. _Unimplemented_.

#### Ordered lists
* `toAscList`. _Unimplemented_.
* `toDescList`. _Unimplemented_.
* `fromAscList`. _Unimplemented_.
* `fromAscListWith`. _Unimplemented_.
* `fromAscListWithKey`. _Unimplemented_.
* `fromDistinctAscList`. _Unimplemented_.

### Filter
* `filter`. Delegated, using `filterWithKey`.
* `filterWithKey`. Raw.
* `partition`. _Unimplemented_.
* `partitionWithKey`. _Unimplemented_.
* `mapMaybe`. Delegated, using `mapMaybeWithKey`.
* `mapMaybeWithKey`. Raw.
* `mapEither`. _Unimplemented_.
* `mapEitherWithKey`. _Unimplemented_.
* `split`. _Unimplemented_.
* `splitLookup`. _Unimplemented_.

### Submap
* `isSubmapOf`. _Unimplemented_.
* `isSubmapOfBy`. _Unimplemented_.
* `isProperSubmapOf`. _Unimplemented_
* `isProperSubmapOfBy`. _Unimplemented_.

### Min/Max
* `findMin`. Raw. Note that this is asymptotically faster than stock `Data.IntMap`.
* `findMax`. Raw. Note that this is asymptotically faster than stock `Data.IntMap`.
* `deleteMin`. Delegated, using `findMin` and `delete`.
* `deleteMax`. Delegated, using `findMin` and `delete`.
* `deleteFindMin`. Delegated, using `findMin` and `delete`.
* `deleteFindMax`. Delegated, using `findMin` and `delete`.
* `updateMin`. Delegated, using `findMin` and `update`.
* `updateMax`. Delegated, using `findMin` and `update`.
* `updateMinWithKey`. Delegated, using `findMin` and `updateWithKey`.
* `updateMaxWithKey`. Delegated, using `findMin` and `updateWithKey`.
* `minView`. Delegated, using `findMin` and `delete`.
* `maxView`. Delegated, using `findMin` and `delete`.
* `minViewWithKey`. Delegated, using `findMin` and `delete`.
* `maxViewWithKey`. Delegated, using `findMin` and `delete`.

### Debugging
Note that this section shouldn't matter to the average user.
* `showTree`. Raw.
* `showTreeWith`. _Unimplemented_.
