{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordMap.Strict (
    -- * Map type
      WordMap, Key

    -- * Operators
    , (!)
    , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered Lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf
    , isSubmapOfBy
    , isProperSubmapOf
    , isProperSubmapOfBy

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- * Debugging
    , showTree
    , valid
) where

import Data.WordMap.Base

import Control.Applicative (Applicative(..))
import Data.Functor ((<$>))
import Data.Bits (xor)

import qualified Data.List (foldl')

import Prelude hiding (foldr, foldl, lookup, null, map, filter, min, max)

(#!), (#) :: (a -> b) -> a -> b
(#!) = ($!)
(#) = ($)

-- | /O(1)/. A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: Key -> a -> WordMap a
singleton k v = v `seq` NonEmpty k v Tip

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
insert :: Key -> a -> WordMap a -> WordMap a
insert = start
  where
    start !k !v Empty = NonEmpty k v Tip
    start !k !v (NonEmpty min minV root)
        | k > min = NonEmpty min minV (goL k v (xor min k) min root)
        | k < min = NonEmpty k v (insertMinL (xor min k) min minV root)
        | otherwise = NonEmpty k v root
    
    goL !k v !_        !_    Tip = Bin k v Tip Tip
    goL !k v !xorCache !min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL k v xorCache min l) r
                    else Bin max maxV l (goR k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max v l r
      where xorCacheMax = xor k max

    goR !k v !_        !_    Tip = Bin k v Tip Tip
    goR !k v !xorCache !max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR k v xorCache max r)
                    else Bin min minV (goL k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min v l r
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: (a -> a -> a) -> Key -> a -> WordMap a -> WordMap a
insertWith combine = start
  where
    start !k v Empty = NonEmpty k #! v # Tip
    start !k v (NonEmpty min minV root)
        | k > min = NonEmpty min minV (goL k v (xor min k) min root)
        | k < min = NonEmpty k #! v # insertMinL (xor min k) min minV root
        | otherwise = NonEmpty k #! combine v minV # root
    
    goL !k v !_        !_    Tip = Bin k #! v # Tip # Tip
    goL !k v !xorCache !min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL k v xorCache min l) r
                    else Bin max maxV l (goR k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k #! v # Bin max maxV l r # Tip
                    else Bin k #! v # l # insertMaxR xorCacheMax max maxV r
        | otherwise = Bin max #! combine v maxV # l # r
      where xorCacheMax = xor k max

    goR !k v !_        !_    Tip = Bin k #! v # Tip # Tip
    goR !k v !xorCache !max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR k v xorCache max r)
                    else Bin min minV (goL k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k #! v # Tip # Bin min minV l r
                    else Bin k #! v # insertMinL xorCacheMin min minV l # r
        | otherwise = Bin min #! combine v minV # l # r
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> WordMap a -> WordMap a
insertWithKey f k = insertWith (f k) k

-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> WordMap a -> (Maybe a, WordMap a)
insertLookupWithKey combine k v = k `seq` start
  where
    start Empty = (Nothing, NonEmpty k #! v # Tip)
    start (NonEmpty min minV root)
        | k > min = let (mv, root') = goL (xor min k) min root
                    in  (mv, NonEmpty min minV root')
        | k < min = (Nothing, NonEmpty k #! v # insertMinL (xor min k) min minV root)
        | otherwise = (Just minV, NonEmpty k #! combine k v minV # root)
    
    goL !_        _    Tip = (Nothing, Bin k #! v # Tip # Tip)
    goL !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then let (mv, l') = goL xorCache min l
                         in  (mv, Bin max maxV l' r)
                    else let (mv, r') = goR xorCacheMax max r
                         in  (mv, Bin max maxV l r')
        | k > max = if xor min max < xorCacheMax
                    then (Nothing, Bin k #! v # Bin max maxV l r # Tip)
                    else (Nothing, Bin k #! v # l # insertMaxR xorCacheMax max maxV r)
        | otherwise = (Just maxV, Bin max #! combine k v maxV # l # r)
      where xorCacheMax = xor k max

    goR !_        _    Tip = (Nothing, Bin k #! v # Tip # Tip)
    goR !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then let (mv, r') = goR xorCache max r
                         in  (mv, Bin min minV l r')
                    else let (mv, l') = goL xorCacheMin min l
                         in  (mv, Bin min minV l' r)
        | k < min = if xor min max < xorCacheMin
                    then (Nothing, Bin k #! v # Tip # Bin min minV l r)
                    else (Nothing, Bin k #! v # insertMinL xorCacheMin min minV l # r)
        | otherwise = (Just minV, Bin min #! combine k v minV # l # r)
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
adjust :: (a -> a) -> Key -> WordMap a -> WordMap a
adjust f k = k `seq` start
  where
    start Empty = Empty
    start m@(NonEmpty min minV node)
        | k > min = NonEmpty min minV (goL (xor min k) min node)
        | k < min = m
        | otherwise = NonEmpty min #! f minV # node
    
    goL !_        _      Tip = Tip
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = Bin max #! f maxV # l # r
      where xorCacheMax = xor k max
    
    goR !_        _      Tip = Tip
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = Bin min #! f minV # l # r
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
adjustWithKey :: (Key -> a -> a) -> Key -> WordMap a -> WordMap a
adjustWithKey f k = adjust (f k) k

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
update :: (a -> Maybe a) -> Key -> WordMap a -> WordMap a
update f k = k `seq` start
  where
    start Empty = Empty
    start m@(NonEmpty min minV Tip)
        | k == min = case f minV of
            Nothing -> Empty
            Just !minV' -> NonEmpty min minV' Tip
        | otherwise = m
    start m@(NonEmpty min minV root@(Bin max maxV l r))
        | k < min = m
        | k == min = case f minV of
            Nothing -> let DR min' minV' root' = deleteMinL max maxV l r
                       in NonEmpty min' minV' root'
            Just !minV' -> NonEmpty min minV' root
        | otherwise = NonEmpty min minV (goL (xor min k) min root)
    
    goL !_        _      Tip = Tip
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case f maxV of
            Nothing -> case r of
                Tip -> l
                Bin minI minVI lI rI -> let DR max' maxV' r' = deleteMaxR minI minVI lI rI
                                        in  Bin max' maxV' l r'
            Just !maxV' -> Bin max maxV' l r
      where xorCacheMax = xor k max
    
    goR !_        _      Tip = Tip
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case f minV of
            Nothing -> case l of
                Tip -> r
                Bin maxI maxVI lI rI -> let DR min' minV' l' = deleteMinL maxI maxVI lI rI
                                        in  Bin min' minV' l' r
            Just !minV' -> Bin min minV' l r
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. The expression (@'updateWithKey' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: (Key -> a -> Maybe a) -> Key -> WordMap a -> WordMap a
updateWithKey f k = update (f k) k

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> WordMap a -> (Maybe a, WordMap a)
updateLookupWithKey f k = k `seq` start
  where
    start Empty = (Nothing, Empty)
    start m@(NonEmpty min minV Tip)
        | k == min = case f min minV of
            Nothing -> (Just minV, Empty)
            Just !minV' -> (Just minV, NonEmpty min minV' Tip)
        | otherwise = (Nothing, m)
    start m@(NonEmpty min minV root@(Bin max maxV l r))
        | k < min = (Nothing, m)
        | k == min = case f min minV of
            Nothing -> let DR min' minV' root' = deleteMinL max maxV l r
                       in (Just minV, NonEmpty min' minV' root')
            Just !minV' -> (Just minV, NonEmpty min minV' root)
        | otherwise = let (mv, root') = goL (xor min k) min root
                      in  (mv, NonEmpty min minV root')
    
    goL !_        _      Tip = (Nothing, Tip)
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then let (mv, l') = goL xorCache min l
                         in  (mv, Bin max maxV l' r)
                    else let (mv, r') = goR xorCacheMax max r
                         in  (mv, Bin max maxV l r')
        | k > max = (Nothing, n)
        | otherwise = case f max maxV of
            Nothing -> case r of
                Tip -> (Just maxV, l)
                Bin minI minVI lI rI -> let DR max' maxV' r' = deleteMaxR minI minVI lI rI
                                        in (Just maxV, Bin max' maxV' l r')
            Just !maxV' -> (Just maxV, Bin max maxV' l r)
      where xorCacheMax = xor k max
    
    goR !_        _      Tip = (Nothing, Tip)
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then let (mv, r') = goR xorCache max r
                         in  (mv, Bin min minV l r')
                    else let (mv, l') = goL xorCacheMin min l
                         in  (mv, Bin min minV l' r)
        | k < min = (Nothing, n)
        | otherwise = case f min minV of
            Nothing -> case l of
                Tip -> (Just minV, r)
                Bin maxI maxVI lI rI -> let DR min' minV' l' = deleteMinL maxI maxVI lI rI
                                        in (Just minV, Bin min' minV' l' r)
            Just !minV' -> (Just minV, Bin min minV' l r)
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> WordMap a -> WordMap a
alter f k m | member k m = update (f . Just) k m
            | otherwise = case f Nothing of
                Just x -> insert k x m
                Nothing -> m

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (a -> a -> a) -> WordMap a -> WordMap a -> WordMap a
unionWith f = unionWithKey (const f)

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: (Key -> a -> a -> a) -> WordMap a -> WordMap a -> WordMap a
unionWithKey combine = start
  where
    start Empty m2 = m2
    start m1 Empty = m1
    start (NonEmpty min1 minV1 root1) (NonEmpty min2 minV2 root2)
        | min1 < min2 = NonEmpty min1 minV1 (goL2 minV2 min1 root1 min2 root2)
        | min1 > min2 = NonEmpty min2 minV2 (goL1 minV1 min1 root1 min2 root2)
        | otherwise = NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 root1 root2 -- we choose min1 arbitrarily, as min1 == min2
    
    -- TODO: Should I bind 'minV1' in a closure? It never changes.
    -- TODO: Should I cache @xor min1 min2@?
    goL1 minV1 min1 Tip !_   Tip = Bin min1 minV1 Tip Tip
    goL1 minV1 min1 Tip min2 n2  = goInsertL1 min1 minV1 (xor min1 min2) min2 n2
    goL1 minV1 min1 n1  min2 Tip = insertMinL (xor min1 min2) min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max2 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min2 min1 < xor min1 max2 -> Bin max2 maxV2 (goL1 minV1 min1 n1 min2 l2) r2 -- we choose min1 arbitrarily - we just need something from tree 1
            | max1 > max2 -> Bin max1 maxV1 l2 (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | max1 < max2 -> Bin max2 maxV2 l2 (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # l2 # goRFused max1 (Bin min1 minV1 l1 r1) r2 -- we choose max1 arbitrarily, as max1 == max2
         EQ | max2 < min1 -> disjoint
            | max1 > max2 -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # goL1 minV1 min1 l1 min2 l2 # goRFused max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
         GT | xor min1 max1 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | otherwise -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 n2) r1
       where
         disjoint = Bin max1 maxV1 n2 (Bin min1 minV1 l1 r1)
    
    -- TODO: Should I bind 'minV2' in a closure? It never changes.
    -- TODO: Should I cache @xor min1 min2@?
    goL2 minV2 !_   Tip min2 Tip = Bin min2 minV2 Tip Tip
    goL2 minV2 min1 Tip min2 n2  = insertMinL (xor min1 min2) min2 minV2 n2
    goL2 minV2 min1 n1  min2 Tip = goInsertL2 min2 minV2 (xor min1 min2) min1 n1
    goL2 minV2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max2 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | otherwise -> Bin max2 maxV2 (goL2 minV2 min1 n1 min2 l2) r2
         EQ | max1 < min2 -> disjoint
            | max1 > max2 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # goL2 minV2 min1 l1 min2 l2 # goRFused max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
         GT | xor min1 max1 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 n2) r1 -- we choose min2 arbitrarily - we just need something from tree 2
            | max1 > max2 -> Bin max1 maxV1 l1 (goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | max1 < max2 -> Bin max2 maxV2 l1 (goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # l1 # goRFused max1 r1 (Bin min2 minV2 l2 r2) -- we choose max1 arbitrarily, as max1 == max2
       where
         disjoint = Bin max2 maxV2 n1 (Bin min2 minV2 l2 r2)
    
    -- TODO: Should I bind 'min' in a closure? It never changes.
    -- TODO: Should I use an xor cache here?
    -- 'goLFused' is called instead of 'goL' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goL'
    goLFused !_ Tip n2 = n2
    goLFused !_ n1 Tip = n1
    goLFused min n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min max1) (xor min max2) of
        LT -> Bin max2 maxV2 (goLFused min n1 l2) r2
        EQ | max1 > max2 -> Bin max1 maxV1 (goLFused min l1 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> Bin max2 maxV2 (goLFused min l1 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # goLFused min l1 l2 # goRFused max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
        GT -> Bin max1 maxV1 (goLFused min l1 n2) r1
    
    -- TODO: Should I bind 'maxV1' in a closure? It never changes.
    -- TODO: Should I cache @xor max1 max2@?
    goR1 maxV1 max1 Tip !_   Tip = Bin max1 maxV1 Tip Tip
    goR1 maxV1 max1 Tip max2 n2  = goInsertR1 max1 maxV1 (xor max1 max2) max2 n2
    goR1 maxV1 max1 n1  max2 Tip = insertMaxR (xor max1 max2) max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max2 `ltMSB` xor max1 max2 -> disjoint -- we choose max1 and max2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min2 max1 > xor max1 max2 -> Bin min2 minV2 l2 (goR1 maxV1 max1 n1 max2 r2) -- we choose max1 arbitrarily - we just need something from tree 1
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2) r2
            | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 (Bin max1 maxV1 l1 r1) l2 # r2 -- we choose min1 arbitrarily, as min1 == min2
         EQ | max1 < min2 -> disjoint
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # goR1 maxV1 max1 r1 max2 r2 -- we choose min1 arbitrarily, as min1 == min2
         GT | xor min1 max1 `ltMSB` xor max1 max2 -> disjoint -- we choose max1 and max2 arbitrarily - we just need something from tree 1 and something from tree 2
            | otherwise -> Bin min1 minV1 l1 (goR1 maxV1 max1 r1 max2 n2)
       where
         disjoint = Bin min1 minV1 (Bin max1 maxV1 l1 r1) n2
    
    -- TODO: Should I bind 'minV2' in a closure? It never changes.
    -- TODO: Should I cache @xor min1 min2@?
    goR2 maxV2 !_   Tip max2   Tip = Bin max2 maxV2 Tip Tip
    goR2 maxV2 max1 Tip max2 n2  = insertMaxR (xor max1 max2) max2 maxV2 n2
    goR2 maxV2 max1 n1  max2 Tip = goInsertR2 max2 maxV2 (xor max1 max2) max1 n1
    goR2 maxV2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max2 `ltMSB` xor max1 max2 -> disjoint -- we choose max1 and max2 arbitrarily - we just need something from tree 1 and something from tree 2
            | otherwise -> Bin min2 minV2 l2 (goR2 maxV2 max1 n1 max2 r2)
         EQ | max2 < min1 -> disjoint
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # goR2 maxV2 max1 r1 max2 r2 -- we choose min1 arbitrarily, as min1 == min2
         GT | xor min1 max1 `ltMSB` xor max1 max2 -> disjoint -- we choose max1 and max2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 maxV2 max1 r1 max2 n2) -- we choose max2 arbitrarily - we just need something from tree 2
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 (Bin max2 maxV2 l2 r2) # r1 -- we choose min1 arbitrarily, as min1 == min2
       where
         disjoint = Bin min2 minV2 (Bin max2 maxV2 l2 r2) n1
    
    -- TODO: Should I bind 'max' in a closure? It never changes.
    -- TODO: Should I use an xor cache here?
    -- 'goRFused' is called instead of 'goR' if the minimums of the two trees are the same
    -- Note that because of this property, the trees cannot be disjoint, so we can skip most of the checks in 'goR'
    goRFused !_ Tip n2 = n2
    goRFused !_ n1 Tip = n1
    goRFused max n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
        LT -> Bin min2 minV2 l2 (goRFused max n1 r2)
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goRFused max r1 r2)
           | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goRFused max r1 r2)
           | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # goRFused max r1 r2 -- we choose min1 arbitrarily, as min1 == min2
        GT -> Bin min1 minV1 l1 (goRFused max r1 n2)
    
    goInsertL1 k v !_        _    Tip = Bin k #! v # Tip # Tip
    goInsertL1 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL1 k v xorCache min l) r
                    else Bin max maxV l (goInsertR1 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max #! combine k v maxV # l # r
      where xorCacheMax = xor k max

    goInsertR1 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR1 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR1 k v xorCache max r)
                    else Bin min minV (goInsertL1 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min #! combine k v minV # l # r
      where xorCacheMin = xor min k
    
    goInsertL2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertL2 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL2 k v xorCache min l) r
                    else Bin max maxV l (goInsertR2 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max #! combine k maxV v # l # r
      where xorCacheMax = xor k max

    goInsertR2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR2 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR2 k v xorCache max r)
                    else Bin min minV (goInsertL2 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min #! combine k minV v # l # r
      where xorCacheMin = xor min k

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: (a -> a -> a) -> [WordMap a] -> WordMap a
unionsWith f = Data.List.foldl' (unionWith f) empty

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: (a -> b -> Maybe a) -> WordMap a -> WordMap b -> WordMap a
differenceWith f = differenceWithKey (const f)

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"
differenceWithKey :: (Key -> a -> b -> Maybe a) -> WordMap a -> WordMap b -> WordMap a
differenceWithKey combine = start
  where
    start Empty !_ = Empty
    start !m Empty = m
    start (NonEmpty min1 minV1 root1) (NonEmpty min2 minV2 root2)
        | min1 < min2 = NonEmpty min1 minV1 (goL2 min1 root1 min2 root2)
        | min1 > min2 = goL1 minV1 min1 root1 min2 root2
        | otherwise = case combine min1 minV1 minV2 of
            Nothing -> goLFused min1 root1 root2
            Just !minV1' -> NonEmpty min1 minV1' (goLFusedKeep min1 root1 root2)
    
    goL1 minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL1 minV1 min1 n1 _ Tip = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> flipBounds $ NonEmpty max1 maxV1 (goR2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> case combine max1 maxV1 maxV2 of
                Nothing -> flipBounds $ goRFused max1 (Bin min1 minV1 l1 r1) r2
                Just !maxV1' -> flipBounds $ NonEmpty max1 maxV1' (goRFusedKeep max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case combine max1 maxV1 maxV2 of
                Nothing -> binL (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2)
                Just !maxV1' -> binL (goL1 minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1' (goRFusedKeep max1 r1 r2))
        GT -> binL (goL1 minV1 min1 l1 min2 n2) (NonEmpty max1 maxV1 r1)
    
    goL2 !_   Tip !_   !_  = Tip
    goL2 min1 n1  min2 Tip = goDeleteL min2 (xor min1 min2) n1
    goL2 _ n1@(Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = n1
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
           | otherwise -> case combine max1 maxV1 maxV2 of
                Nothing -> case goRFused max1 r1 r2 of
                    Empty -> goL2 min1 l1 min2 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
                Just !maxV1' -> Bin max1 maxV1' (goL2 min1 l1 min2 l2) (goRFusedKeep max1 r1 r2)
        GT | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 min1 l1 min2 n2) r1 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> Bin max1 maxV1 l1 (goR2 max1 r1 max2 (Bin min2 dummyV l2 r2))
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
           | otherwise -> case combine max1 maxV1 maxV2 of
                Nothing -> case goRFused max1 r1 (Bin min2 dummyV l2 r2) of
                    Empty -> l1
                    NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
                Just !maxV1' -> Bin max1 maxV1' l1 (goRFusedKeep max1 r1 (Bin min2 dummyV l2 r2))
    
    goLFused min = loop
      where
        loop Tip !_ = Empty
        loop (Bin max1 maxV1 l1 r1) Tip = case deleteMinL max1 maxV1 l1 r1 of
            DR min' minV' n' -> NonEmpty min' minV' n'
        loop n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> loop n1 l2
            EQ | max1 > max2 -> binL (loop l1 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
               | max1 < max2 -> binL (loop l1 l2) (goR1 maxV1 max1 r1 max2 r2)
               | otherwise -> case combine max1 maxV1 maxV2 of
                    Nothing -> binL (loop l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
                    Just !maxV1' -> binL (loop l1 l2) (NonEmpty max1 maxV1' (goRFusedKeep max1 r1 r2))
            GT -> binL (loop l1 n2) (NonEmpty max1 maxV1 r1)
    
    goLFusedKeep min = loop
      where
        loop n1 Tip = n1
        loop Tip !_ = Tip
        loop n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> loop n1 l2
            EQ | max1 > max2 -> Bin max1 maxV1 (loop l1 l2) (goR2 max1 r1 max2 r2)
               | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                    Empty -> loop l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
               | otherwise -> case combine max1 maxV1 maxV2 of
                    Nothing -> case goRFused max1 r1 r2 of -- we choose max1 arbitrarily, as max1 == max2
                        Empty -> loop l1 l2
                        NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
                    Just !maxV1' -> Bin max1 maxV1' (loop l1 l2) (goRFusedKeep max1 r1 r2)
            GT -> Bin max1 maxV1 (loop l1 n2) r1
    
    goR1 maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR1 maxV1 max1 n1 _ Tip = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> flipBounds $ NonEmpty min1 minV1 (goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2)
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> case combine min1 minV1 minV2 of
                Nothing -> flipBounds $ goLFused min1 (Bin max1 maxV1 l1 r1) l2
                Just !minV1' -> flipBounds $ NonEmpty min1 minV1' (goLFusedKeep min1 (Bin max1 maxV1 l1 r1) l2)
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case combine min1 minV1 minV2 of
                Nothing -> binR (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2)
                Just !minV1' -> binR (NonEmpty min1 minV1' (goLFusedKeep min1 l1 l2)) (goR1 maxV1 max1 r1 max2 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goR1 maxV1 max1 r1 max2 n2)
    
    goR2 !_   Tip !_   !_  = Tip
    goR2 max1 n1  max2 Tip = goDeleteR max2 (xor max1 max2) n1
    goR2 _ n1@(Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = n1
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2) 
           | otherwise -> case combine min1 minV1 minV2 of
                Nothing -> case goLFused min1 l1 l2 of
                    Empty -> goR2 max1 r1 max2 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2) 
                Just !minV1' -> Bin min1 minV1' (goLFusedKeep min1 l1 l2) (goR2 max1 r1 max2 r2)
        GT | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 max1 r1 max2 n2) -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)) r1
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1
           | otherwise -> case combine min1 minV1 minV2 of
                Nothing -> case goLFused min1 l1 (Bin max2 dummyV l2 r2) of
                    Empty -> r1
                    NonEmpty min' minV' l' -> Bin min' minV' l' r1
                Just !minV1' -> Bin min1 minV1' (goLFusedKeep min1 l1 (Bin max2 dummyV l2 r2)) r1
    
    goRFused max = loop
      where
        loop Tip !_ = Empty
        loop (Bin min1 minV1 l1 r1) Tip = case deleteMaxR min1 minV1 l1 r1 of
            DR max' maxV' n' -> NonEmpty max' maxV' n'
        loop n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> loop n1 r2
            EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (loop r1 r2)
               | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (loop r1 r2)
               | otherwise -> case combine min1 minV1 minV2 of
                    Nothing -> binR (goLFused min1 r1 r2) (loop r1 r2) -- we choose min1 arbitrarily, as min1 == min2
                    Just !minV1' -> binR (NonEmpty min1 minV1' (goLFusedKeep min1 r1 r2)) (loop r1 r2)
            GT -> binR (NonEmpty min1 minV1 l1) (loop r1 n2)
    
    goRFusedKeep max = loop
      where
        loop n1 Tip = n1
        loop Tip !_ = Tip
        loop n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> loop n1 r2
            EQ | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 l2) (loop r1 r2)
               | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                    Empty -> loop r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
               | otherwise -> case combine min1 minV1 minV2 of -- we choose min1 arbitrarily, as min1 == min2
                    Nothing -> case goLFused min1 r1 r2 of
                        Empty -> loop r1 r2
                        NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
                    Just !minV1' -> Bin min1 minV1' (goLFusedKeep min1 r1 r2) (loop r1 r2)
            GT -> Bin min1 minV1 l1 (loop r1 n2)
    
    goLookupL k v !_ Tip = NonEmpty k v Tip
    goLookupL k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = NonEmpty k v Tip
        | otherwise = case combine k v maxV of
            Nothing -> Empty
            Just !v' -> NonEmpty k v' Tip
      where xorCacheMax = xor k max
    
    goLookupR k v !_ Tip = NonEmpty k v Tip
    goLookupR k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = NonEmpty k v Tip
        | otherwise = case combine k v minV of
            Nothing -> Empty
            Just !v' -> NonEmpty k v' Tip
      where xorCacheMin = xor min k
    
    goDeleteL _ !_           Tip = Tip
    goDeleteL k !xorCache n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goDeleteL k xorCache l) r
                    else Bin max maxV l (goDeleteR k xorCacheMax r)
        | k > max = n
        | otherwise = case r of
            Tip -> l
            Bin minI minVI lI rI -> let DR max' maxV' r' = deleteMaxR minI minVI lI rI
                                    in  Bin max' maxV' l r'
      where xorCacheMax = xor k max
    
    goDeleteR _ !_           Tip = Tip
    goDeleteR k !xorCache n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goDeleteR k xorCache r)
                    else Bin min minV (goDeleteL k xorCacheMin l) r
        | k < min = n
        | otherwise = case l of
            Tip -> r
            Bin maxI maxVI lI rI -> let DR min' minV' l' = deleteMinL maxI maxVI lI rI
                                    in  Bin min' minV' l' r
      where xorCacheMin = xor min k
    
    dummyV = error "impossible"

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
intersectionWith :: (a -> b -> c) -> WordMap a -> WordMap b -> WordMap c
intersectionWith f = intersectionWithKey (const f)

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"
intersectionWithKey :: (Key -> a -> b -> c) -> WordMap a -> WordMap b -> WordMap c
intersectionWithKey combine = start
  where
    start Empty !_ = Empty
    start !_ Empty = Empty
    start (NonEmpty min1 minV1 root1) (NonEmpty min2 minV2 root2)
        | min1 < min2 = goL2 minV2 min1 root1 min2 root2
        | min1 > min2 = goL1 minV1 min1 root1 min2 root2
        | otherwise = NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 root1 root2 -- we choose min1 arbitrarily, as min1 == min2
    
    -- TODO: This scheme might produce lots of unnecessary flipBounds calls. This should be rectified.
    
    goL1 _     !_   !_  !_   Tip = Empty
    goL1 minV1 min1 Tip min2 n2  = goLookupL1 min1 minV1 (xor min1 min2) n2
    goL1 _ min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = Empty
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> flipBounds $ goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> flipBounds $ NonEmpty max1 #! combine max1 maxV1 maxV2 # goRFused max1 (Bin min1 minV1 l1 r1) r2
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> flipBounds (NonEmpty max1 #! combine max1 maxV1 maxV2 # goRFused max1 r1 r2)
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 #! combine max1 maxV1 maxV2 # l' # goRFused max1 r1 r2)
        GT -> goL1 minV1 min1 l1 min2 n2
    
    goL2 _     !_   Tip !_   !_  = Empty
    goL2 minV2 min1 n1  min2 Tip = goLookupL2 min2 minV2 (xor min1 min2) n1
    goL2 _ _ (Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = Empty
    goL2 minV2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 minV2 min1 n1 min2 l2
        EQ | max1 > max2 -> binL (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL2 minV2 min1 l1 min2 l2 of
                Empty -> flipBounds (NonEmpty max1 #! combine max1 maxV1 maxV2 # goRFused max1 r1 r2)
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 #! combine max1 maxV1 maxV2 # l' # goRFused max1 r1 r2)
        GT | xor min1 min2 < xor min2 max1 -> goL2 minV2 min1 l1 min2 n2 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> flipBounds $ goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2)
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2)
           | otherwise -> flipBounds $ NonEmpty max1 #! combine max1 maxV1 maxV2 # goRFused max1 r1 (Bin min2 minV2 l2 r2)
    
    goLFused min = loop
      where
        loop Tip !_ = Tip
        loop !_ Tip = Tip
        loop n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> loop n1 l2
            EQ | max1 > max2 -> case goR2 maxV2 max1 r1 max2 r2 of
                    Empty -> loop l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
               | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                    Empty -> loop l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
               | otherwise -> Bin max1 #! combine max1 maxV1 maxV2 # loop l1 l2 # goRFused max1 r1 r2 -- we choose max1 arbitrarily, as max1 == max2
            GT -> loop l1 n2
    
    goR1 _     !_   !_  !_   Tip = Empty
    goR1 maxV1 max1 Tip max2 n2  = goLookupR1 max1 maxV1 (xor max1 max2) n2
    goR1 _ max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = Empty
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> flipBounds $ goL2 minV2 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> flipBounds $ NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 (Bin max1 maxV1 l1 r1) l2
        EQ | min1 < min2 -> binR (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> flipBounds (NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2)
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # r')
        GT -> goR1 maxV1 max1 r1 max2 n2
    
    goR2 _     !_   Tip !_   !_  = Empty
    goR2 maxV2 max1 n1  max2 Tip = goLookupR2 max2 maxV2 (xor max1 max2) n1
    goR2 _ _ (Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = Empty
    goR2 maxV2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 maxV2 max1 n1 max2 r2
        EQ | min1 < min2 -> binR (goL2 minV2 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
           | otherwise -> case goR2 maxV2 max1 r1 max2 r2 of
                Empty -> flipBounds (NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2)
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # r')
        GT | xor min1 max2 > xor max2 max1 -> goR2 maxV2 max1 r1 max2 n2 -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> flipBounds $ goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)
           | otherwise -> flipBounds $ NonEmpty min1 #! combine min1 minV1 minV2 # goLFused min1 l1 (Bin max2 maxV2 l2 r2)
    
    goRFused max = loop
      where
        loop Tip !_ = Tip
        loop !_ Tip = Tip
        loop n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 minV2 l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> loop n1 r2
            EQ | min1 < min2 -> case goL2 minV2 min1 l1 min2 l2 of
                    Empty -> loop r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
               | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                    Empty -> loop r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
               | otherwise -> Bin min1 #! combine min1 minV1 minV2 # goLFused min1 l1 l2 # loop r1 r2 -- we choose max1 arbitrarily, as max1 == max2
            GT -> loop r1 n2
    
    goLookupL1 !_ _ !_ Tip = Empty
    goLookupL1 k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL1 k v xorCache l
                    else goLookupR1 k v xorCacheMax r
        | k > max = Empty
        | otherwise = NonEmpty k #! combine k v maxV # Tip
      where xorCacheMax = xor k max
    
    goLookupR1 !_ _ !_ Tip = Empty
    goLookupR1 k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR1 k v xorCache r
                    else goLookupL1 k v xorCacheMin l
        | k < min = Empty
        | otherwise = NonEmpty k #! combine k v minV # Tip
      where xorCacheMin = xor min k
    
    goLookupL2 !_ _ !_ Tip = Empty
    goLookupL2 k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL2 k v xorCache l
                    else goLookupR2 k v xorCacheMax r
        | k > max = Empty
        | otherwise = NonEmpty k #! combine k maxV v # Tip
      where xorCacheMax = xor k max
    
    goLookupR2 !_ _ !_ Tip = Empty
    goLookupR2 k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR2 k v xorCache r
                    else goLookupL2 k v xorCacheMin l
        | k < min = Empty
        | otherwise = NonEmpty k #! combine k minV v # Tip
      where xorCacheMin = xor min k

-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) -> WordMap a -> WordMap b
map f = start
  where
    start Empty = Empty
    start (NonEmpty min minV root) = NonEmpty min #! f minV # go root
    
    go Tip = Tip
    go (Bin k v l r) = Bin k #! f v # go l # go r

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Key -> a -> b) -> WordMap a -> WordMap b
mapWithKey f = start
  where
    start Empty = Empty
    start (NonEmpty min minV root) = NonEmpty min #! f min minV # go root
    
    go Tip = Tip
    go (Bin k v l r) = Bin k #! f k v # go l # go r


-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative f => (Key -> a -> f b) -> WordMap a -> f (WordMap b)
traverseWithKey f = start
  where
    start  Empty = pure Empty
    start (NonEmpty min minV root) = NonEmpty min <$> f min minV <*> goL root
    
    goL  Tip = pure Tip
    goL (Bin max maxV l r) = (\l' r' maxV' -> Bin max #! maxV' # l' # r') <$> goL l <*> goR r <*> f max maxV
    
    goR  Tip = pure Tip
    goR (Bin min minV l r) = (\minV' l' r' -> Bin min #! minV' # l' # r') <$> f min minV <*> goL l <*> goR r

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a -> b -> (a, c)) -> a -> WordMap b -> (a, WordMap c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> WordMap b -> (a, WordMap c)
mapAccumWithKey f = start
  where
    start a  Empty = (a, Empty)
    start a (NonEmpty min minV root) =
        let (a', !minV') = f a min minV
            (a'', root') = goL root a'
        in  (a'', NonEmpty min minV' root')
    
    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',    l') = goL l a
            (a'',   r') = goR r a'
            (a''', !maxV') = f a'' max maxV
        in  (a''', Bin max maxV' l' r')
    
    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',   !minV') = f a min minV
            (a'',   l') = goL l a'
            (a''',  r') = goR r a''
        in  (a''', Bin min minV' l' r')

-- | /O(n)/. The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a, c)) -> a -> WordMap b -> (a, WordMap c)
mapAccumRWithKey f = start
  where
    start a Empty = (a, Empty)
    start a (NonEmpty min minV root) = 
        let (a',   root') = goL root a
            (a'', !minV') = f a' min minV
        in  (a'', NonEmpty min minV' root')
    
    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',  !maxV') = f a max maxV
            (a'',  r') = goR r a'
            (a''', l') = goL l a''
        in  (a''', Bin max maxV' l' r')
    
    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',    r') = goR r a
            (a'',   l') = goL l a'
            (a''', !minV') = f a'' min minV
        in  (a''', Bin min minV' l' r')

-- | /O(n*min(n,W))/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
mapKeys :: (Key -> Key) -> WordMap a -> WordMap a
mapKeys f = foldlWithKey' (\m k a -> insert (f k) a m) empty

-- | /O(n*min(n,W))/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
mapKeysWith :: (a -> a -> a) -> (Key -> Key) -> WordMap a -> WordMap a
mapKeysWith combine f = foldlWithKey' (\m k a -> insertWith combine (f k) a m) empty

-- | /O(n*min(n,W))/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has slightly better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
mapKeysMonotonic :: (Key -> Key) -> WordMap a -> WordMap a
mapKeysMonotonic = mapKeys

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key, a)] -> WordMap a
fromList = Data.List.foldl' (\t (k, a) -> insert k a t) empty

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
-- > fromListWith (++) [] == empty
fromListWith :: (a -> a -> a) -> [(Key, a)] -> WordMap a
fromListWith f = Data.List.foldl' (\t (k, a) -> insertWith f k a t) empty

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty
fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> WordMap a
fromListWithKey f = Data.List.foldl' (\t (k, a) -> insertWithKey f k a t) empty

-- TODO: Use the ordering

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscList :: [(Key, a)] -> WordMap a
fromAscList = fromList

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscListWith :: (a -> a -> a) -> [(Key, a)] -> WordMap a
fromAscListWith = fromListWith

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> WordMap a
fromAscListWithKey = fromListWithKey

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: [(Key, a)] -> WordMap a
fromDistinctAscList = fromList

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: (a -> Maybe b) -> WordMap a -> WordMap b
mapMaybe f = mapMaybeWithKey (const f)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
mapMaybeWithKey :: (Key -> a -> Maybe b) -> WordMap a -> WordMap b
mapMaybeWithKey f = start
  where
    start Empty = Empty
    start (NonEmpty min minV root) = case f min minV of
        Just !minV' -> NonEmpty min minV' (goL root)
        Nothing -> goDeleteL root
    
    goL Tip = Tip
    goL (Bin max maxV l r) = case f max maxV of
        Just !maxV' -> Bin max maxV' (goL l) (goR r)
        Nothing -> case goDeleteR r of
            Empty -> goL l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goL l) r'
    
    goR Tip = Tip
    goR (Bin min minV l r) = case f min minV of
        Just !minV' -> Bin min minV' (goL l) (goR r)
        Nothing -> case goDeleteL l of
            Empty -> goR r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goR r)
    
    goDeleteL Tip = Empty
    goDeleteL (Bin max maxV l r) = case f max maxV of
        Just !maxV' -> case goDeleteL l of
            Empty -> case goR r of
                Tip -> NonEmpty max maxV' Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV' lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV' l' (goR r))
        Nothing -> binL (goDeleteL l) (goDeleteR r)
    
    goDeleteR Tip = Empty
    goDeleteR (Bin min minV l r) = case f min minV of
        Just !minV' -> case goDeleteR r of
            Empty -> case goL l of
                Tip -> NonEmpty min minV' Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV' lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV' (goL l) r')
        Nothing -> binR (goDeleteL l) (goDeleteR r)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
mapEither :: (a -> Either b c) -> WordMap a -> (WordMap b, WordMap c)
mapEither f = mapEitherWithKey (const f)

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
mapEitherWithKey :: (Key -> a -> Either b c) -> WordMap a -> (WordMap b, WordMap c)
mapEitherWithKey func = start
  where
    start Empty = (Empty, Empty)
    start (NonEmpty min minV root) = case func min minV of
        Left !v  -> let SP t f = goTrueL root
                    in (NonEmpty min v t, f)
        Right !v -> let SP t f = goFalseL root
                    in (t, NonEmpty min v f)
    
    goTrueL Tip = SP Tip Empty
    goTrueL (Bin max maxV l r) = case func max maxV of
        Left !v  -> let SP tl fl = goTrueL l
                        SP tr fr = goTrueR r
                    in SP (Bin max v tl tr) (binL fl fr)
        Right !v -> let SP tl fl = goTrueL l
                        SP tr fr = goFalseR r
                        t = case tr of
                            Empty -> tl
                            NonEmpty max' maxV' r' -> Bin max' maxV' tl r'
                        f = case fl of
                            Empty -> flipBounds $ NonEmpty max v fr
                            NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max v l' fr)
                    in SP t f
    
    goTrueR Tip = SP Tip Empty
    goTrueR (Bin min minV l r) = case func min minV of
        Left !v  -> let SP tl fl = goTrueL l
                        SP tr fr = goTrueR r
                    in SP (Bin min v tl tr) (binR fl fr)
        Right !v -> let SP tl fl = goFalseL l
                        SP tr fr = goTrueR r
                        t = case tl of
                            Empty -> tr
                            NonEmpty min' minV' l' -> Bin min' minV' l' tr
                        f = case fr of
                            Empty -> flipBounds $ NonEmpty min v fl
                            NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min v fl r')
                    in SP t f
    
    goFalseL Tip = SP Empty Tip
    goFalseL (Bin max maxV l r) = case func max maxV of
        Left !v  -> let SP tl fl = goFalseL l
                        SP tr fr = goTrueR r
                        t = case tl of
                            Empty -> flipBounds $ NonEmpty max v tr
                            NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max v l' tr)
                        f = case fr of
                            Empty -> fl
                            NonEmpty max' maxV' r' -> Bin max' maxV' fl r'
                    in SP t f
        Right !v -> let SP tl fl = goFalseL l
                        SP tr fr = goFalseR r
                    in SP (binL tl tr) (Bin max v fl fr)
    
    goFalseR Tip = SP Empty Tip
    goFalseR (Bin min minV l r) = case func min minV of
        Left !v  -> let SP tl fl = goTrueL l
                        SP tr fr = goFalseR r
                        t = case tr of
                            Empty -> flipBounds $ NonEmpty min v tl
                            NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min v tl r')
                        f = case fl of
                            Empty -> fr
                            NonEmpty min' minV' l' -> Bin min' minV' l' fr
                    in SP t f
        Right !v -> let SP tl fl = goFalseL l
                        SP tr fr = goFalseR r
                    in SP (binR tl tr) (Bin min v fl fr)

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMin :: (a -> Maybe a) -> WordMap a -> WordMap a
updateMin _ Empty = Empty
updateMin f m = update f (fst (findMin m)) m

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMax :: (a -> Maybe a) -> WordMap a -> WordMap a
updateMax _ Empty = Empty
updateMax f m = update f (fst (findMax m)) m

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMinWithKey :: (Key -> a -> Maybe a) -> WordMap a -> WordMap a
updateMinWithKey _ Empty = Empty
updateMinWithKey f m = updateWithKey f (fst (findMin m)) m

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMaxWithKey :: (Key -> a -> Maybe a) -> WordMap a -> WordMap a
updateMaxWithKey _ Empty = Empty
updateMaxWithKey f m = updateWithKey f (fst (findMax m)) m
