{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordSet.Internal where

import Control.DeepSeq

import qualified Data.List as List

import Data.Word (Word)
import Data.Bits (xor)

import Prelude hiding (foldr, foldl, lookup, null, map)

type Key = Word

data WordSet = NonEmpty {-# UNPACK #-} !Key !Node | Empty deriving (Eq)
data Node = Bin {-# UNPACK #-} !Key !Node !Node | Tip deriving (Eq, Show)

instance Show WordSet where
    show m = "fromList " ++ show (toList m)

instance NFData WordSet

-- | /O(1)/. Is the map empty?
null :: WordSet -> Bool
null Empty = True
null _ = False

-- | /O(n)/. Number of elements in the map.
size :: WordSet -> Int
size Empty = 0
size (NonEmpty _ node) = sizeNode node where
    sizeNode Tip = 1
    sizeNode (Bin _ l r) = sizeNode l + sizeNode r

-- TODO: Is there a good way to unify the 'lookup'-like functions?

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> WordSet -> Bool
member k = k `seq` start
  where
    start Empty = False
    start (NonEmpty min node)
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min Tip = False
    goL !xorCache min (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache min l
                    else goR xorCacheMax max r
        | k > max = False
        | otherwise = True
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = False
    goR !xorCache max (Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache max r
                    else goL xorCacheMin min l
        | k < min = False
        | otherwise = True
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Is the key not a member of the map?
notMember :: Key -> WordSet -> Bool
notMember k = k `seq` start
  where
    start Empty = True
    start (NonEmpty min node)
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min Tip = True
    goL !xorCache min (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache min l
                    else goR xorCacheMax max r
        | k > max = True
        | otherwise = False
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = True
    goR !xorCache max (Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache max r
                    else goL xorCacheMin min l
        | k < min = True
        | otherwise = False
      where xorCacheMin = xor min k

{-
-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Key -> WordMap a -> Maybe (Key, a)
lookupLT k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | min >= k = Nothing
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min (Tip x)
        | min < k = Just (min, x)
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | max < k = Just (max, getMaxV r)
        | xorCache < xorCacheMax = goL xorCache min l
        | otherwise = goR xorCacheMax max r min l
      where
        xorCacheMax = xor k max
    
    goR !xorCache max (Tip x) fMin fallback
        | max < k = Just (max, x)
        | otherwise = Just (getMax fMin fallback)
    goR !xorCache max (Bin min l r) fMin fallback
        | min >= k = Just (getMax fMin fallback)
        | xorCache < xorCacheMin = goR xorCache max r min l
        | otherwise = goL xorCacheMin min l
      where
        xorCacheMin = xor min k
    
    getMax min (Tip x) = (min, x)
    getMax min (Bin max l r) = (max, getMaxV r)
    
    getMaxV (Tip x) = x
    getMaxV (Bin b l r) = getMaxV r

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Key -> WordMap a -> Maybe (Key, a)
lookupLE k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | min > k = Nothing
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min (Tip x)
        | min <= k = Just (min, x)
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | max <= k = Just (max, getMaxV r)
        | xorCache < xorCacheMax = goL xorCache min l
        | otherwise = goR xorCacheMax max r min l
      where
        xorCacheMax = xor k max
    
    goR !xorCache max (Tip x) fMin fallback
        | max <= k = Just (max, x)
        | otherwise = Just (getMax fMin fallback)
    goR !xorCache max (Bin min l r) fMin fallback
        | min > k = Just (getMax fMin fallback)
        | xorCache < xorCacheMin = goR xorCache max r min l
        | otherwise = goL xorCacheMin min l
      where
        xorCacheMin = xor min k
    
    getMax min (Tip x) = (min, x)
    getMax min (Bin max l r) = (max, getMaxV r)
    
    getMaxV (Tip x) = x
    getMaxV (Bin b l r) = getMaxV r

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Key -> WordMap a -> Maybe (Key, a)
lookupGT k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min (Tip x)) = Just (min, x)
    start (NonEmpty min (Bin max l r))
        | max <= k = Nothing
        | otherwise = goR (xor k max) max (Bin min l r)
    
    goL !xorCache min (Tip x) fMax fallback
        | min > k = Just (min, x)
        | otherwise = Just (getMin fMax fallback)
    goL !xorCache min (Bin max l r) fMax fallback
        | max <= k = Just (getMin fMax fallback)
        | xorCache < xorCacheMax = goL xorCache min l max r
        | otherwise = goR xorCacheMax max r
      where
        xorCacheMax = xor k max
    
    goR !xorCache max (Tip x)
        | max > k = Just (max, x)
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | min > k = Just (min, getMinV l)
        | xorCache < xorCacheMin = goR xorCache max r
        | otherwise = goL xorCacheMin min l max r
      where
        xorCacheMin = xor min k
    
    getMin max (Tip x) = (max, x)
    getMin max (Bin min l r) = (min, getMinV l)
    
    getMinV (Tip x) = x
    getMinV (Bin b l r) = getMinV l

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Key -> WordMap a -> Maybe (Key, a)
lookupGE k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min (Tip x)) = Just (min, x)
    start (NonEmpty min (Bin max l r))
        | max < k = Nothing
        | otherwise = goR (xor k max) max (Bin min l r)
    
    goL !xorCache min (Tip x) fMax fallback
        | min >= k = Just (min, x)
        | otherwise = Just (getMin fMax fallback)
    goL !xorCache min (Bin max l r) fMax fallback
        | max < k = Just (getMin fMax fallback)
        | xorCache < xorCacheMax = goL xorCache min l max r
        | otherwise = goR xorCacheMax max r
      where
        xorCacheMax = xor k max
    
    goR !xorCache max (Tip x)
        | max >= k = Just (max, x)
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | min >= k = Just (min, getMinV l)
        | xorCache < xorCacheMin = goR xorCache max r
        | otherwise = goL xorCacheMin min l max r
      where
        xorCacheMin = xor min k
    
    getMin max (Tip x) = (max, x)
    getMin max (Bin min l r) = (min, getMinV l)
    
    getMinV (Tip x) = x
    getMinV (Bin b l r) = getMinV l
   -} 
-- | /O(1)/. The empty map.
empty :: WordSet
empty = Empty

-- | /O(1)/. A map of one element.
singleton :: Key -> WordSet
singleton k = NonEmpty k Tip

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value
-- is replaced with the supplied value. 
insert :: Key -> WordSet -> WordSet
insert k = k `seq` start
  where
    start Empty = NonEmpty k Tip
    start n@(NonEmpty min root)
        | k > min = NonEmpty min (goL (xor min k) min root)
        | k < min = NonEmpty k (endL (xor min k) min root)
        | otherwise = n
    
    goL !xorCache min Tip = Bin k Tip Tip
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k (Bin max l r) Tip
                    else Bin k l (endR xorCacheMax max r)
        | otherwise = n
      where xorCacheMax = xor k max

    goR !xorCache max Tip = Bin k Tip Tip
    goR !xorCache max n@(Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min l (goR xorCache max r)
                    else Bin min (goL xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k Tip (Bin min l r)
                    else Bin k (endL xorCacheMin min l) r
        | otherwise = n
      where xorCacheMin = xor min k
    
    endL !xorCache min = finishL
      where
        finishL Tip = Bin min Tip Tip
        finishL (Bin max l r)
            | xor min max < xorCache = Bin max Tip (Bin min l r)
            | otherwise = Bin max (finishL l) r

    endR !xorCache max = finishR
      where
        finishR Tip = Bin max Tip Tip
        finishR (Bin min l r)
            | xor min max < xorCache = Bin min (Bin max l r) Tip
            | otherwise = Bin min l (finishR r)

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> WordSet -> WordSet
delete k = k `seq` start
  where
    start Empty = Empty
    start m@(NonEmpty min Tip)
        | k == min = Empty
        | otherwise = m
    start m@(NonEmpty min root@(Bin max l r))
        | k < min = m
        | k == min = let DR min' root' = goDeleteMin max l r in NonEmpty min' root'
        | otherwise = NonEmpty min (goL (xor min k) min root)
    
    goL !xorCache min Tip = Tip
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case r of
            Tip -> l
            Bin minI lI rI -> let DR max' r' = goDeleteMax minI lI rI
                              in  Bin max' l r'
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = Tip
    goR !xorCache max n@(Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min l (goR xorCache max r)
                    else Bin min (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case l of
            Tip -> r
            Bin maxI lI rI -> let DR min' l' = goDeleteMin maxI lI rI
                              in  Bin min' l' r
      where xorCacheMin = xor min k
    
    goDeleteMin max l r = case l of
        Tip -> case r of
            Tip -> DR max r
            Bin min l' r' -> DR min (Bin max l' r')
        Bin maxI lI rI -> let DR min l' = goDeleteMin maxI lI rI
                          in  DR min (Bin max l' r)
    
    goDeleteMax min l r = case r of
        Tip -> case l of
            Tip -> DR min l
            Bin max l' r' -> DR max (Bin min l' r')
        Bin minI lI rI -> let DR max r' = goDeleteMax minI lI rI
                          in  DR max (Bin min l r')

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.WordMap)
data DeleteResult = DR {-# UNPACK #-} !Key !Node

map :: (Key -> Key) -> WordSet -> WordSet
map f = fromList . List.map f . toList

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (Key -> b -> b) -> b -> WordSet -> b
foldr f z = start
  where
    start Empty = z
    start (NonEmpty min root) = f min (goL root z)
    
    goL Tip acc = acc
    goL (Bin max l r) acc = goL l (goR r (f max acc))
    
    goR Tip acc = acc
    goR (Bin min l r) acc = f min (goL l (goR r acc))

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> Key -> a) -> a -> WordSet -> a
foldl f z = start
  where
    start Empty = z
    start (NonEmpty min root) = goL root (f z min)
    
    goL Tip acc = acc
    goL (Bin max l r) acc = f (goR r (goL l acc)) max
    
    goR Tip acc = acc
    goR (Bin min l r) acc = goR r (goL l (f acc min))

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> WordSet -> b
foldr' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = f min (goL root z)
    
    goL Tip !acc = acc
    goL (Bin max l r) !acc = goL l (goR r (f max acc))
    
    goR Tip !acc = acc
    goR (Bin min l r) !acc = f min (goL l (goR r acc))

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> WordSet -> a
foldl' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = goL root (f z min)
    
    goL Tip !acc = acc
    goL (Bin max l r) !acc = f (goR r (goL l acc)) max
    
    goR Tip !acc = acc
    goR (Bin min l r) !acc = goR r (goL l (f acc min))

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [Key] -> WordSet
fromList = List.foldr insert empty

-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending order.
elems :: WordSet -> [Key]
elems = toAscList

-- | /O(n)/. Convert the set to a list of elements.
toList :: WordSet -> [Key]
toList = toAscList

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: WordSet -> [Key]
toAscList = foldr (:) []

-- | /O(n)/. Convert the set to a descending list of elements.
toDescList :: WordSet -> [Key]
toDescList = foldl (flip (:)) []
{-
-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: (a -> Bool) -> WordMap a -> WordMap a
filter p = filterWithKey (const p)

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: (Key -> a -> Bool) -> WordMap a -> WordMap a
filterWithKey p = start
  where
    start Empty = Empty
    start (NonEmpty min root) = goL min root
    
    goL min (Tip x)
        | p min x = NonEmpty min (Tip x)
        | otherwise = Empty
    goL min (Bin max l r) = binL (goL min l) (goR max r)
    
    goR max (Tip x)
        | p max x = NonEmpty max (Tip x)
        | otherwise = Empty
    goR max (Bin min l r) = binR (goL min l) (goR max r)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: (a -> Bool) -> WordMap a -> (WordMap a, WordMap a)
partition p = partitionWithKey (const p)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: (Key -> a -> Bool) -> WordMap a -> (WordMap a, WordMap a)
partitionWithKey p = start
  where
    start Empty = Empty
    start (NonEmpty min root) = goL min root
    
    goL min (Tip x)
        | p min x = (NonEmpty min (Tip x), Empty)
        | otherwise = (Empty, NonEmpty min (Tip x))
    goL min (Bin max l r) = let (lt, lf) = goL min l
                                (rt, rf) = goR max r
                            in  (binL lt rt, binR lf rf)
    
    goR max (Tip x)
        | p max x = (NonEmpty max (Tip x), Empty)
        | otherwise = (Empty, NonEmpty max (Tip x))
    goR max (Bin min l r) = let (lt, lf) = goL min l
                                (rt, rf) = goR max r
                            in  (binL lt rt, binR lf rf)

splitLookup :: Key -> WordMap a -> (WordMap a, Maybe a, WordMap a)
splitLookup k = k `seq` start
  where
    start Empty = (Empty, Nothing, Empty)
    start (NonEmpty min root)
        | k < min = (Empty, Nothing, NonEmpty min root)
        | otherwise = goL (xor min k) min root
    
    goL !xorCache min (Tip x)
        | k == min = (Empty, Just x, Empty)
        | otherwise = (NonEmpty min (Tip x), Nothing, Empty)
    goL !xorCache min n@(Bin max l r)
        | k > max = (NonEmpty min n, Nothing, Empty)
        | xorCache < xorCacheMax = let (ll, lv, lr) = goL xorCache min l
                                   in  (ll, lv, Bin max lr r)
        | otherwise              = let (rl, rv, rr) = goR xorCacheMax max r
                                   in  (Bin max l rl, rv, rr)
      where
        xorCacheMax = xor k max
-}

-- | /O(1)/. The minimal element of the set.
findMin :: WordSet -> Word
findMin Empty = error "findMin: empty set has no minimal element"
findMin (NonEmpty min _) = min

-- | /O(1)/. The maximal element of a set.
findMax :: WordSet -> Word
findMax Empty = error "findMax: empty set has no maximal element"
findMax (NonEmpty min Tip) = min
findMax (NonEmpty _ (Bin max _ _)) = max

----------------------------

-- | Show the tree that implements the map.
showTree :: WordSet -> String
showTree = unlines . aux where
    aux Empty = []
    aux (NonEmpty min node) = show min : auxNode False node
    auxNode _ Tip = ["+-."]
    auxNode lined (Bin bound l r) = ["+--" ++ show bound, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent r = prefix : "  " ++ r

valid :: WordSet -> Bool
valid Empty = True
valid (NonEmpty min root) = allKeys (> min) root && goL min root
  where
    goL min Tip = True
    goL min (Bin max l r) =
           allKeys (< max) l
        && allKeys (< max) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    goR max Tip = True
    goR max (Bin min l r) =
           allKeys (> min) l
        && allKeys (> min) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    allKeys p Tip = True
    allKeys p (Bin b l r) = p b && allKeys p l && allKeys p r
