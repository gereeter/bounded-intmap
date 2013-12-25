bounded-intmap
==============

`bounded-intmap` is a reimplementation of `Data.IntMap` that uses minimum and maximum bounds on subtrees instread of bit prefixes. The original idea, by Edward Kmett, is described [here](https://www.fpcomplete.com/user/edwardk/revisiting-matrix-multiplication/part-4). As per my current benchmark results, this implemenation seems to range from 33% faster to 50% slower than stock `Data.IntMap`. Note that only two functions in the benchmark, `insert` and `intersectionWithKey`, are slower than stock `Data.IntMap`, and even then `insert` is only slower if the key is not already present in the map.

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
### The basic integer map: the bitwise trie ###
We are trying to create an efficient, simple mapping from integers to values. The most common approaches for these are hash tables, which are not persistent (though we can come close with HAMTs), and binary search trees, which work well, but don't use any special properties of the integer. To come up with this mapping, we need to think of integers not as numbers, but instead as strings of bits. Once we change our mindset, we can use the standard _trie_ data structure to build our mapping. As bits are particularly simple, so is the resulting structure:

```haskell
data WordMap a = Bin (WordMap a) (WordMap a) | Tip a | Nil
```

The `Bin` constructor represents a bitwise branch, and the `Tip` constructor comes after (on my machine) 64 `Bin` construtors in the tree. The associated basic operations are fairly simple:

```haskell
lookup :: Word -> WordMap a -> Maybe a
lookup k = go 0
  where
    go b (Bin l r) = if testBit b k
                     then go (b + 1) l
                     else go (b + 1) r
    go _ (Tip x) = Just x
    go _ Nil = Nothing

insert :: Word -> a -> WordMap a -> WordMap a
insert k a = go 0
  where
    go 64 _ = Tip a
    go b (Bin l r) = if testBit b k
                     then Bin (go (b + 1) l) r
                     else Bin l (go (b + 1) r)
    go b _ = if testBit b k
             then Bin (go (b + 1) Nil) Nil
             else Bin Nil (go (b + 1) Nil)
```

`delete` follows similarly, and `union` isn't to hard - I leave it as an exercise to the reader. Unfortunately, this approach is horribly slow and space efficient. To see why, let us look at the tree structure for `singleton 5 "hello"`:

```
\0
 \0
  \0
   \0
    \0
     \0
      \0
       \0
        \0
         \0
          \0
           \0
            \0
           1/
            \0
           1/
         "hello"
```

Note that, for brevity, I have shortened the word size to 16 bits - the diagram is 4 times larger for our 64 bit system. In this atrocious tree structure, there is one pointer for every bit - a 64 fold explosion in space. Arguably worse is the fact that every single `lookup` or `insert` or `delete` must traverse 64 pointers, resulting in 64 cache misses and a terrible runtime. So, how do we fix this?

### Path compression: PATRICIA trees and stock `Data.IntMap` ###
The key observation to reducing the space usage is that we can compress nodes that only have one child together - since they form a linear chain, we can simply concatenate the bits within that chain. For example, again temporarily shortening the word size to 16 bits:

```
singleton 5 "hello":

| 0000000000000101
"hello"

fromList [(1, "1"), (4, "4"), (5, "5")]:

     | 0000000000000___
001/  \10_
"1"  0/ \1
    "4" "5"
```

This clearly produces a much more space efficient structure, and the basic operations, while more complicated, are still straightforward. In Haskell, the structure is:

```haskell
data WordMap a = Bin Prefix Mask (WordMap a) (WordMap a) | Tip Word a | Nil
```

Note that in the above representation, the `Mask` is used to tell how long the `Prefix` is, and the `Word` in the `Tip` nodes is to avoid the for using `Bin` for singletons. This final representation is known as the big-endian PATRICIA tree, and is what today's `Data.IntMap` uses internally, albeit with some optimizations like strictness and unpacking, which I have omitted for simplicity. However, we can take this structure a few steps farther, which is the goal of this package.

### Implicit prefixes: a simpler representation ###

The central observation for this step comes from Edward Kmett, as mentioned in a previous section. In the PATRICIA tree representation, we explicitly stored the common prefix of all the keys in a subtree. However, this prefix is not needed if we know what the largest and smallest keys stored within a subtree are - the common prefix of all the keys is just the common prefix of the minimum and maximum keys. Using this observation, we get another representation:

```haskell
data WordMap a = Bin Word Word (WordMap a) (WordMap a) | Tip Word a | Nil
```

In tree form:
```
singleton 5 hello:

| 5
"hello"

fromList [(1, "1"), (4, "4"), (5, "5")]:

    | (1, 5)
  1/  \ (4, 5)
"1"  4/ \5
    "4"  "5"
```

Traversing this tree efficiently is a bit more difficult, but still possible. For details, see the section below titled "Figuring out which way to go". This representation, since it gives exact minimums and maximums, can actually be more efficient than the PATRICIA tree, as seaches can terminate earlier. The range of values between the minimum and maximum is generally smaller than the range of values with the correct prefix, and so searches will know earlier if they are going to fail. However, the big gains of this representation come after a few more steps.

### Removing redundancy ###
You may have noticed that the above representation store many keys repeatedly - in the {1,4,5} example, 1 was stored twice, 4 was stored twice, and 5 was stored three times. The reason for this is very simple. In the {1,4,5} example, we knew that the minimum was 1 and the maximum was 5. At the first branch, we split the set into two parts - {1} and {4,5}. However, the minimum of the smaller set was exactly the minimum of the original set. Similarly, the maximum of the larger set was exactly the maximum of the original set. Since we always travers the tree downward, this information is not needed. We can restructure the tree to only store 1 new value at each branch, removing the redundancy. Note that we also have to add an extra value at the root node, where this transformation does not work. In summary:

```haskell
data WordMap a = Empty | NonEmpty Word (Node a)
data Node a = Bin Word (Node a) (Node a) | Tip a
```

In tree form:
```
    | 1
    | 5
   / \
"1" 4/ \
   "4"  "5"
```

With this optimization, the operations get more complicated again, but we have achieved something amazing - this new representation is more memory efficient than stock `Data.IntMap`. We will improve this again, as well as the runtime, with our final optimization.

### Moving the values upward ###
If you look carefully at the tree structure from the previous section, you will notice that we removed the redundancy perfectly - every key is stored exactly once. However, if the keys are stored in a unique location in the tree, why are the values stored far away? We can move the values upward in the tree to pair them with their keys and so get a simpler structure.

In Haskell:
```haskell
data WordMap a = Empty | NonEmpty Word a (Node a)
data Node a = Bin Word a (Node a) (Node a) | Tip
```

In tree form:
```
    | 1 "1"
    | 5 "5"
   / \
     / \ 4 "4"
```

At first, this seems to improve neither runtime nor space usage - after all, all we did was move the values around. However, the `Tip` constructor is now empty, meaning that it can be shared among all the leaves of every tree. The `Tip` constructor essentiall disappears from the space usage profile, and we get a gain in memory. The runtime effect is even larger. Because the values are now high in the tree, functions like `lookup` don't have to go all the way to the leaves. This means following fewer pointers, which means fewer cache misses and just a shorter loop. Admittedly, after all this work, our functions have become much larger than the sizes they started with, but we have won speed gains and significant memory gains from the current state of the art.

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
* `insertLookupWithKey`. Raw.

#### Delete/Update
* `delete`. Raw.
* `adjust`. Raw.
* `adjustWithkey`. Delegated, using `adjust`.
* `update`. Raw.
* `updateWithKey`. Delegated, using `update`.
* `updateLookupWithKey`. Raw.
* `alter`. Delegated, using `member` and either `update` or `insert`.

### Combine
#### Union
* `union`. Delegated, using `unionWith`.
* `unionWith`. Delegated, using `unionWithKey`.
* `unionWithKey`. Raw.
* `unions`. Delegated, using lots of `union`s.
* `unionsWith`. Delegated, using lots of `unionWith`s.

#### Difference
* `difference`. Delegated, using `foldrWithKey'` and lots of `delete`s.
* `differenceWith`. Delegated, using `differenceWithKey`.
* `differenceWithKey`. Delegated, using `foldrWithKey'` and lots of `update`s.

#### Intersection
* `intersection`. Delegated, using `intersectionWith`.
* `intersectionWith`. Delegated, using `intersectionWithKey`.
* `intersectionWithKey`. Raw. Note that it is still slower than stock `Data.IntMap` by up to (though not necessarily) 50%.

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
* `elems`. Delegated, using `foldr`.
* `keys`. Delegated, using `foldrWithKey`.
* `assocs`. Delegated, using `toAscList`.
* `keysSet`. _Unimplemented_. Note that I'm not sure whether to convert to stock `Data.IntSet` or `Data.WordSet`, which is much more in flux than `Data.WordMap`.
* `fromSet`. _Unimplemented_. Note that I'm not sure whether to convert from stock `Data.IntSet` or `Data.WordSet`, which is much more in flux than `Data.WordMap`.

#### Lists
* `toList`. Delegated, using `toAscList`.
* `fromList`. Delegated, using lots of `insert`s.
* `fromListWith`. Delegated, using lots of `insert`s.
* `fromListWithKey`. Delegated, using lots of `insert`s.

#### Ordered lists
* `toAscList`. Delegated, using `foldrWithKey`.
* `toDescList`. Delegated, using `foldlWithKey`.
* `fromAscList`. Delegated, using `fromList`.
* `fromAscListWith`. Delegated, using `fromListWith`.
* `fromAscListWithKey`. Delegated, using `fromListWithKey`.
* `fromDistinctAscList`. Delegated, using `fromList`.

### Filter
* `filter`. Delegated, using `filterWithKey`.
* `filterWithKey`. Raw.
* `partition`. Delegated, using `partitionWithKey`.
* `partitionWithKey`. Raw.
* `mapMaybe`. Delegated, using `mapMaybeWithKey`.
* `mapMaybeWithKey`. Raw.
* `mapEither`. Delegated, using `mapEitherWithKey`.
* `mapEitherWithKey`. Raw.
* `split`. Delegated, using `splitLookup`.
* `splitLookup`. Raw.

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
