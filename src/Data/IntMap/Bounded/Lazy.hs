{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.IntMap.Bounded.Lazy (
    -- * Map type
      IntMap, Key
    
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
    
    -- ** Folds
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

import Data.Functor ((<$>))
import Control.Applicative (Applicative(..))

import Data.Bits (xor)

import Data.IntMap.Bounded.Base

import Data.WordMap.Base (WordMap(..), Node(..))
import qualified Data.WordMap.Lazy as W

import qualified Data.List (foldl')

import Prelude hiding (foldr, foldl, lookup, null, map, filter, min, max)

-- | /O(1)/. A map of one element.
singleton :: Key -> a -> IntMap a
singleton k v = IntMap (W.singleton (i2w k) v)

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value
-- is replaced with the supplied value.
insert :: Key -> a -> IntMap a -> IntMap a
insert k v (IntMap m) = IntMap (W.insert (i2w k) v m)

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith combine k v (IntMap m) = IntMap (W.insertWith combine (i2w k) v m)

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
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey combine k v (IntMap m) = IntMap (W.insertWithKey (combine . w2i) (i2w k) v m)

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
insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey combine k v (IntMap m) =
    let (mv, m') = W.insertLookupWithKey (combine . w2i) (i2w k) v m
    in (mv, IntMap m')

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
adjust :: (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k (IntMap m) = IntMap (W.adjust f (i2w k) m)

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f k (IntMap m) = IntMap (W.adjustWithKey (f . w2i) (i2w k) m)

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f k (IntMap m) = IntMap (W.update f (i2w k) m)

-- | /O(min(n,W))/. The expression (@'updateWithKey' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k (IntMap m) = IntMap (W.updateWithKey (f . w2i) (i2w k) m)

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a, IntMap a)
updateLookupWithKey f k (IntMap m) =
    let (mv, m') = W.updateLookupWithKey (f . w2i) (i2w k) m
    in (mv, IntMap m')

-- | /O(min(n,W))/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'IntMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f k (IntMap m) = IntMap (W.alter f (i2w k) m)

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f (IntMap m1) (IntMap m2) = IntMap (W.unionWith f m1 m2)

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.unionWithKey (f . w2i) m1 m2)

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: (a -> a -> a) -> [IntMap a] -> IntMap a
unionsWith f = Data.List.foldl' (unionWith f) empty

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f (IntMap m1) (IntMap m2) = IntMap (W.differenceWith f m1 m2)

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"
differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey f (IntMap m1) (IntMap m2) = IntMap (W.differenceWithKey (f . w2i) m1 m2)

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWith f m1 m2)

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"
intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWithKey (f . w2i) m1 m2)

-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) -> IntMap a -> IntMap b
map f (IntMap m) = IntMap (W.map f m)

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f (IntMap m) = IntMap (W.mapWithKey (f . w2i) m)

-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative f => (Key -> a -> f b) -> IntMap a -> f (IntMap b)
traverseWithKey f = start
  where
    start (IntMap Empty) = pure (IntMap Empty)
    start (IntMap (NonEmpty min minV Tip)) = (\minV' -> IntMap (NonEmpty min minV' Tip)) <$> f (w2i min) minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            (\r' maxV' minV' l' -> IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
            <$> goR r <*> f (w2i max) maxV <*> f (w2i min) minV <*> goL l
        | otherwise =
            (\minV' l' r' maxV' -> IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
            <$> f (w2i min) minV <*> goL l <*> goR r <*> f (w2i max) maxV 
    
    goL Tip = pure Tip
    goL (Bin max maxV l r) = (\l' r' v' -> Bin max v' l' r') <$> goL l <*> goR r <*> f (w2i max) maxV
    
    goR Tip = pure Tip
    goR (Bin min minV l r) = Bin min <$> f (w2i min) minV <*> goL l <*> goR r

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
mapAccum :: (a -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
mapAccumWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccumWithKey f = start
  where
    start a (IntMap Empty) = (a, IntMap Empty)
    start a (IntMap (NonEmpty min minV Tip)) =
        let (a', minV') = f a (w2i min) minV
        in  (a', IntMap (NonEmpty min minV' Tip))
    start a (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            let (a',    r')    = goR r a
                (a'',   maxV') = f a' (w2i max) maxV
                (a''',  minV') = f a'' (w2i min) minV
                (a'''', l')    = goL l a'''
            in  (a'''', IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
        | otherwise =
            let (a',    minV') = f a (w2i min) minV
                (a'',   l')    = goL l a'
                (a''',  r')    = goR r a''
                (a'''', maxV') = f a''' (w2i max) maxV
            in  (a'''', IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
    
    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',   l') = goL l a
            (a'',  r') = goR r a'
            (a''', maxV') = f a'' (w2i max) maxV
        in  (a''', Bin max maxV' l' r')
    
    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',   minV') = f a (w2i min) minV
            (a'',   l') = goL l a'
            (a''',  r') = goR r a''
        in  (a''', Bin min minV' l' r')

-- | /O(n)/. The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a, c)) -> a -> IntMap b -> (a, IntMap c)
mapAccumRWithKey f = start
  where
    start a (IntMap Empty) = (a, IntMap Empty)
    start a (IntMap (NonEmpty min minV Tip)) =
        let (a', minV') = f a (w2i min) minV
        in  (a', IntMap (NonEmpty min minV' Tip))
    start a (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            let (a',    l')    = goL l a
                (a'',   minV') = f a' (w2i min) minV
                (a''',  maxV') = f a'' (w2i max) maxV
                (a'''', r')    = goR r a'''
            in  (a'''', IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
        | otherwise =
            let (a',    maxV') = f a (w2i max) maxV
                (a'',   r')    = goR r a'
                (a''',  l')    = goL l a''
                (a'''', minV') = f a''' (w2i min) minV
            in  (a'''', IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
    
    goL  Tip a = (a, Tip)
    goL (Bin max maxV l r) a =
        let (a',   maxV') = f a (w2i max) maxV
            (a'',  r') = goR r a'
            (a''', l') = goL l a''
        in  (a''', Bin max maxV' l' r')
    
    goR  Tip a = (a, Tip)
    goR (Bin min minV l r) a =
        let (a',   r') = goR r a
            (a'',  l') = goL l a'
            (a''', minV') = f a'' (w2i min) minV
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
mapKeys :: (Key -> Key) -> IntMap a -> IntMap a
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
mapKeysWith :: (a -> a -> a) -> (Key -> Key) -> IntMap a -> IntMap a
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
mapKeysMonotonic :: (Key -> Key) -> IntMap a -> IntMap a
mapKeysMonotonic = mapKeys

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key, a)] -> IntMap a
fromList = Data.List.foldl' (\t (k, a) -> insert k a t) empty

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
-- > fromListWith (++) [] == empty
fromListWith :: (a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWith f = Data.List.foldl' (\t (k, a) -> insertWith f k a t) empty

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty
fromListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> IntMap a
fromListWithKey f = Data.List.foldl' (\t (k, a) -> insertWithKey f k a t) empty

-- TODO: Use the ordering

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscList :: [(Key, a)] -> IntMap a
fromAscList = fromList

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
fromAscListWith :: (a -> a -> a) -> [(Key, a)] -> IntMap a
fromAscListWith = fromListWith

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key, a)] -> IntMap a
fromAscListWithKey = fromListWithKey

-- | /O(n)/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
fromDistinctAscList :: [(Key, a)] -> IntMap a
fromDistinctAscList = fromList

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f (IntMap m) = IntMap (W.mapMaybe f m)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (IntMap m) = IntMap (W.mapMaybeWithKey (f . w2i) m)

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f (IntMap m) = let (m1, m2) = W.mapEither f m in (IntMap m1, IntMap m2)

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f (IntMap m) = let (m1, m2) = W.mapEitherWithKey (f . w2i) m in (IntMap m1, IntMap m2)

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMin :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMin _ (IntMap Empty) = IntMap Empty
updateMin f m = update f (fst (findMin m)) m

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMax :: (a -> Maybe a) -> IntMap a -> IntMap a
updateMax _ (IntMap Empty) = IntMap Empty
updateMax f m = update f (fst (findMax m)) m

-- | /O(min(n,W))/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateMinWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMinWithKey _ (IntMap Empty) = IntMap Empty
updateMinWithKey f m = updateWithKey f (fst (findMin m)) m

-- | /O(min(n,W))/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
updateMaxWithKey :: (Key -> a -> Maybe a) -> IntMap a -> IntMap a
updateMaxWithKey _ (IntMap Empty) = IntMap Empty
updateMaxWithKey f m = updateWithKey f (fst (findMax m)) m
