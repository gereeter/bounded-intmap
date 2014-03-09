{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.IntMap.Bounded.Base where

import Control.DeepSeq (NFData(..))
import Control.Applicative (Applicative(..))

import Data.Monoid (Monoid(..))
import qualified Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))

import Data.Functor ((<$>))

import Data.Word (Word)
import Data.Bits (xor)

import Data.WordMap.Base (WordMap(..), Node(..))
import qualified Data.WordMap.Base as W

import qualified Data.IntSet (IntSet, fromDistinctAscList)

import Prelude hiding (foldr, foldl, lookup, null, map, min, max)

type Key = Int

newtype IntMap a = IntMap (W.WordMap a)

instance Show a => Show (IntMap a) where
    show m = "fromList " ++ show (toList m)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)

instance Data.Foldable.Foldable IntMap where
    foldr = foldr
    foldr' = foldr'
    foldl = foldl
    foldl' = foldl'

instance Traversable IntMap where
    traverse f = start
      where
        start (IntMap Empty) = pure (IntMap Empty)
        start (IntMap (NonEmpty min minV Tip)) = (\minV' -> IntMap (NonEmpty min minV' Tip)) <$> f minV
        start (IntMap (NonEmpty min minV (Bin max maxV l r)))
            | w2i (xor min max) < 0 =
                (\r' maxV' minV' l' -> IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
                <$> goR r <*> f maxV <*> f minV <*> goL l
            | otherwise =
                (\minV' l' r' maxV' -> IntMap (NonEmpty min minV' (Bin max maxV' l' r')))
                <$> f minV <*> goL l <*> goR r <*> f maxV 
        
        goL Tip = pure Tip
        goL (Bin max maxV l r) = (\l' r' v' -> Bin max v' l' r') <$> goL l <*> goR r <*> f maxV
        
        goR Tip = pure Tip
        goR (Bin min minV l r) = Bin min <$> f minV <*> goL l <*> goR r

instance Monoid (IntMap a) where
    mempty = empty
    mappend = union

instance NFData a => NFData (IntMap a) where
    rnf (IntMap m) = rnf m

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'
(!) :: IntMap a -> Key -> a
(!) m k = findWithDefault (error $ "IntMap.!: key " ++ show k ++ " is not an element of the map") k m

-- | Same as 'difference'.
(\\) :: IntMap a -> IntMap b -> IntMap a
IntMap m1 \\ IntMap m2 = IntMap (m1 W.\\ m2)

-- | /O(1)/. Is the map empty?
null :: IntMap a -> Bool
null (IntMap m) = W.null m

-- | /O(n)/. Number of elements in the map.
size :: IntMap a -> Int
size (IntMap m) = W.size m

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> IntMap a -> Bool
member k (IntMap m) = W.member (i2w k) m

-- | /O(min(n,W))/. Is the key not a member of the map?
notMember :: Key -> IntMap a -> Bool
notMember k (IntMap m) = W.notMember (i2w k) m

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Key -> IntMap a -> Maybe a
lookup k (IntMap m) = W.lookup (i2w k) m

-- | /O(min(n,W))/. The expression @findWithDefault def k map@ returns
-- the value at key @k@ or returns @def@ when the key is not an element
-- of the map. 
findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k (IntMap m) = W.findWithDefault def (i2w k) m

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Key -> IntMap a -> Maybe (Key, a)
lookupLT k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | w2i min < k = Just (w2i min, minV)
        | otherwise = Nothing
    start (IntMap (NonEmpty min minV node@(Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            if w2i min < k -- if this is true, we know the result is positive
            then Just (goL (xor min (i2w k)) min minV l)
            else if w2i max < k -- if this is true, k is between the positives and negatives
                 then Just (w2i max, maxV)
                 else case r of
                        Tip -> Nothing
                        Bin minI minVI lI rI
                            | minI >= i2w k -> Nothing
                            | otherwise -> Just (goL (xor minI (i2w k)) minI minVI (Bin max maxV lI rI))
        | min < i2w k = Just (goL (xor min (i2w k)) min minV node)
        | otherwise = Nothing
    
    goL !_ min minV Tip = (w2i min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max < i2w k = (w2i max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor (i2w k) max
    
    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min >= i2w k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min (i2w k)
    
    getMax min minV Tip = (w2i min, minV)
    getMax _   _   (Bin max maxV _ _) = (w2i max, maxV)

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Key -> IntMap a -> Maybe (Key, a)
lookupLE k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | w2i min <= k = Just (w2i min, minV)
        | otherwise = Nothing
    start (IntMap (NonEmpty min minV node@(Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            if w2i min <= k -- if this is true, we know the result is positive
            then Just (goL (xor min (i2w k)) min minV l)
            else if w2i max <= k -- if this is true, k is between the positives and negatives
                 then Just (w2i max, maxV)
                 else case r of
                        Tip -> Nothing
                        Bin minI minVI lI rI
                            | minI > i2w k -> Nothing
                            | otherwise -> Just (goL (xor minI (i2w k)) minI minVI (Bin max maxV lI rI))
        | min <= i2w k = Just (goL (xor min (i2w k)) min minV node)
        | otherwise = Nothing
    
    goL !_ min minV Tip = (w2i min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max <= i2w k = (w2i max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor (i2w k) max
    
    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min > i2w k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min (i2w k)
    
    getMax min minV Tip = (w2i min, minV)
    getMax _   _   (Bin max maxV _ _) = (w2i max, maxV)

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Key -> IntMap a -> Maybe (Key, a)
lookupGT k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | w2i min > k = Just (w2i min, minV)
        | otherwise = Nothing
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            if w2i max > k -- if this is true, we know the result is negative
            then Just (goR (xor (i2w k) max) max maxV r)
            else if w2i min > k -- if this is true, k is between the positives and negatives
                 then Just (w2i min, minV)
                 else case l of
                        Tip -> Nothing
                        Bin maxI maxVI lI rI
                            | maxI <= i2w k -> Nothing
                            | otherwise -> Just (goR (xor (i2w k) maxI) maxI maxVI (Bin min minV lI rI))
        | max > i2w k = Just (goR (xor (i2w k) max) max maxV (Bin min minV l r))
        | otherwise = Nothing
    
    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max <= i2w k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor (i2w k) max
    
    goR !_ max maxV Tip = (w2i max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min > i2w k = (w2i min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min (i2w k)
    
    getMin max maxV Tip = (w2i max, maxV)
    getMin _   _   (Bin min minV _ _) = (w2i min, minV)

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Key -> IntMap a -> Maybe (Key, a)
lookupGE k = k `seq` start
  where
    start (IntMap Empty) = Nothing
    start (IntMap (NonEmpty min minV Tip))
        | w2i min >= k = Just (w2i min, minV)
        | otherwise = Nothing
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 =
            if w2i max >= k -- if this is true, we know the result is negative
            then Just (goR (xor (i2w k) max) max maxV r)
            else if w2i min >= k -- if this is true, k is between the positives and negatives
                 then Just (w2i min, minV)
                 else case l of
                        Tip -> Nothing
                        Bin maxI maxVI lI rI
                            | maxI < i2w k -> Nothing
                            | otherwise -> Just (goR (xor (i2w k) maxI) maxI maxVI (Bin min minV lI rI))
        | max >= i2w k = Just (goR (xor (i2w k) max) max maxV (Bin min minV l r))
        | otherwise = Nothing
    
    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max < i2w k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor (i2w k) max
    
    goR !_ max maxV Tip = (w2i max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min >= i2w k = (w2i min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min (i2w k)
    
    getMin max maxV Tip = (w2i max, maxV)
    getMin _   _   (Bin min minV _ _) = (w2i min, minV)

-- | /O(1)/. The empty map.
empty :: IntMap a
empty = IntMap W.empty

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> IntMap a -> IntMap a
delete k (IntMap m) = IntMap (W.delete (i2w k) m)

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: IntMap a -> IntMap a -> IntMap a
union (IntMap m1) (IntMap m2) = IntMap (W.union m1 m2)

-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: [IntMap a] -> IntMap a
unions = Data.Foldable.foldl' union empty

-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
difference :: IntMap a -> IntMap b -> IntMap a
difference (IntMap m1) (IntMap m2) = IntMap (W.difference m1 m2)

-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: IntMap a -> IntMap b -> IntMap a
intersection (IntMap m1) (IntMap m2) = IntMap (W.intersection m1 m2)

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> IntMap a -> b
foldr f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV Tip)) = f minV z
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goR r (f maxV (f minV (goL l z)))
        | otherwise = f minV (goL l (goR r (f maxV z)))
    
    goL Tip acc = acc
    goL (Bin _ maxV l r) acc = goL l (goR r (f maxV acc))
    
    goR Tip acc = acc
    goR (Bin _ minV l r) acc = f minV (goL l (goR r acc))

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> IntMap b -> a
foldl f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV Tip)) = f z minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goL (f (f (goR z r) maxV) minV) l
        | otherwise = f (goR (goL (f z minV) l) r) maxV

    goL acc Tip = acc
    goL acc (Bin _ maxV l r) = f (goR (goL acc l) r) maxV
    
    goR acc Tip = acc
    goR acc (Bin _ minV l r) = goR (goL (f acc minV) l) r

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
{-# INLINE foldrWithKey #-}
foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV Tip)) = f' min minV z
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goR r (f' max maxV (f' min minV (goL l z)))
        | otherwise = f' min minV (goL l (goR r (f' max maxV z)))
    
    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l (goR r (f' max maxV acc))
    
    goR Tip acc = acc
    goR (Bin min minV l r) acc = f' min minV (goL l (goR r acc))
    
    f' k a b = f (w2i k) a b

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
{-# INLINE foldlWithKey #-}
foldlWithKey :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV Tip)) = f' z min minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goL (f' (f' (goR z r) max maxV) min minV) l
        | otherwise = f' (goR (goL (f' z min minV) l) r) max maxV

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = f' (goR (goL acc l) r) max maxV
    
    goR acc Tip = acc
    goR acc (Bin min minV l r) = goR (goL (f' acc min minV) l) r
    
    f' a k b = f a (w2i k) b

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = start
  where
    start (IntMap Empty) = mempty
    start (IntMap (NonEmpty min minV Tip)) = f (w2i min) minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goR r `mappend` f (w2i max) maxV `mappend` f (w2i min) minV `mappend` goL l
        | otherwise = f (w2i min) minV `mappend` goL l `mappend` goR r `mappend` f (w2i max) maxV
    
    goL Tip = mempty
    goL (Bin max maxV l r) = goL l `mappend` goR r `mappend` f (w2i max) maxV
    
    goR Tip = mempty
    goR (Bin min minV l r) = f (w2i min) minV `mappend` goL l `mappend` goR r

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
{-# INLINE foldr' #-}
foldr' :: (a -> b -> b) -> b -> IntMap a -> b
foldr' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV Tip)) = f minV $! z
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goR r $! f maxV $! f minV $! goL l $! z
        | otherwise = f minV $! goL l $! goR r $! f maxV $! z
    
    goL Tip acc = acc
    goL (Bin _ maxV l r) acc = goL l $! goR r $! f maxV $! acc
    
    goR Tip acc = acc
    goR (Bin _ minV l r) acc = f minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
{-# INLINE foldl' #-}
foldl' :: (a -> b -> a) -> a -> IntMap b -> a
foldl' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty _ minV Tip)) = f z minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = s goL (s f (s f (s goR z r) maxV) minV) l
        | otherwise = s f (s goR (s goL (s f z minV) l) r) maxV

    goL acc Tip = acc
    goL acc (Bin _ maxV l r) = s f (s goR (s goL acc l) r) maxV
    
    goR acc Tip = acc
    goR acc (Bin _ minV l r) = s goR (s goL (s f acc minV) l) r
    
    s = ($!)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
{-# INLINE foldrWithKey' #-}
foldrWithKey' :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV Tip)) = f' min minV $! z
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = goR r $! f' max maxV $! f' min minV $! goL l $! z
        | otherwise = f' min minV $! goL l $! goR r $! f' max maxV $! z
    
    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l $! goR r $! f' max maxV $! acc
    
    goR Tip acc = acc
    goR (Bin min minV l r) acc = f' min minV $! goL l $! goR r $! acc
    
    f' k a b = f (w2i k) a b

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
{-# INLINE foldlWithKey' #-}
foldlWithKey' :: (a -> Key -> b -> a) -> a -> IntMap b -> a
foldlWithKey' f z = start
  where
    start (IntMap Empty) = z
    start (IntMap (NonEmpty min minV Tip)) = s f' z min minV
    start (IntMap (NonEmpty min minV (Bin max maxV l r)))
        | w2i (xor min max) < 0 = s goL (s f' (s f' (s goR z r) max maxV) min minV) l
        | otherwise = s f' (s goR (s goL (s f' z min minV) l) r) max maxV

    goL acc Tip = acc
    goL acc (Bin max maxV l r) = s f' (s goR (s goL acc l) r) max maxV
    
    goR acc Tip = acc
    goR acc (Bin min minV l r) = s goR (s goL (s f' acc min minV) l) r
    
    f' a k b = f a (w2i k) b
    
    s = ($!)

-- TODO: make the conversion functions good producers

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
elems :: IntMap a -> [a]
elems = foldr (:) []

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
keys :: IntMap a -> [Key]
keys = foldrWithKey (\k _ l -> k : l) []

-- | /O(n)/. An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
assocs :: IntMap a -> [(Key, a)]
assocs = toAscList

-- | /O(n*min(n,W))/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty
keysSet :: IntMap a -> Data.IntSet.IntSet
keysSet = Data.IntSet.fromDistinctAscList . keys

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntMap a -> [(Key, a)]
toList = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: IntMap a -> [(Key, a)]
toAscList = foldrWithKey (\k v l -> (k, v) : l) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]
toDescList :: IntMap a -> [(Key, a)]
toDescList = foldlWithKey (\l k v -> (k, v) : l) []

-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p (IntMap m) = IntMap (W.filter p m)

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p (IntMap m) = IntMap (W.filterWithKey (p . w2i) m)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p (IntMap m) = let (m1, m2) = W.partition p m in (IntMap m1, IntMap m2)

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p (IntMap m) = let (m1, m2) = W.partitionWithKey (p . w2i) m in (IntMap m1, IntMap m2)

-- | /O(min(n,W))/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
split :: Key -> IntMap a -> (IntMap a, IntMap a)
split k m
    | k < 0 = let (glb, lub) = W.split (i2w k) neg
              in (IntMap glb, IntMap (W.binL nonneg (W.flipBounds lub)))
    | otherwise = let (glb, lub) = W.split (i2w k) nonneg
                  in (IntMap (W.binL glb (W.flipBounds neg)), IntMap lub)
  where
    (neg, nonneg) = split0 m

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Key -> IntMap a -> (IntMap a, Maybe a, IntMap a)
splitLookup k m
    | k < 0 = let (glb, eq, lub) = W.splitLookup (i2w k) neg
              in (IntMap glb, eq, IntMap (W.binL nonneg (W.flipBounds lub)))
    | otherwise = let (glb, eq, lub) = W.splitLookup (i2w k) nonneg
                  in (IntMap (W.binL glb (W.flipBounds neg)), eq, IntMap lub)
  where
    (neg, nonneg) = split0 m

-- | /O(1)/.  Decompose a map into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6::Int] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d'),(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
{-# INLINE splitRoot #-}
splitRoot :: IntMap a -> [IntMap a]
splitRoot (IntMap Empty) = []
splitRoot m@(IntMap (NonEmpty _ _ Tip)) = [m]
splitRoot (IntMap (NonEmpty min minV (Bin max maxV l r)))
    | w2i (xor min max) < 0 = [IntMap (W.flipBounds (NonEmpty max maxV r)), IntMap (NonEmpty min minV l)]
    | otherwise = [IntMap (NonEmpty min minV l), IntMap (W.flipBounds (NonEmpty max maxV r))]

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
IntMap m1 `isSubmapOf` IntMap m2 = m1 `W.isSubmapOf` m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}
isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfBy f (IntMap m1) (IntMap m2) = W.isSubmapOfBy f m1 m2

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isProperSubmapOf (IntMap m1) (IntMap m2) = W.isProperSubmapOf m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
@m1@ and @m2@ are not equal,
all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
applied to their respective values. For example, the following
expressions are all 'True':

> isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
> isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

But the following are all 'False':

> isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
> isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
> isProperSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isProperSubmapOfBy p (IntMap m1) (IntMap m2) = W.isProperSubmapOfBy p m1 m2

-- | /O(1)/. The minimal key of the map.
findMin :: IntMap a -> (Key, a)
findMin (IntMap Empty) = error "findMin: empty map has no minimal element"
findMin (IntMap (NonEmpty min minV Tip)) = (w2i min, minV)
findMin (IntMap (NonEmpty min minV (Bin max maxV _ r)))
    | w2i (xor min max) < 0 = case r of
        Tip -> (w2i max, maxV)
        Bin min' minV' _ _ -> (w2i min', minV')
    | otherwise = (w2i min, minV)

-- | /O(1)/. The maximal key of the map.
findMax :: IntMap a -> (Key, a)
findMax (IntMap Empty) = error "findMax: empty map has no maximal element"
findMax (IntMap (NonEmpty min minV Tip)) = (w2i min, minV)
findMax (IntMap (NonEmpty min minV (Bin max maxV l _)))
    | w2i (xor min max) < 0 = case l of
        Tip -> (w2i min, minV)
        Bin max' maxV' _ _ -> (w2i max', maxV')
    | otherwise = (w2i max, maxV) 

-- | /O(min(n,W))/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMin :: IntMap a -> IntMap a
deleteMin (IntMap Empty) = IntMap Empty
deleteMin m = delete (fst (findMin m)) m

-- | /O(min(n,W))/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMax :: IntMap a -> IntMap a
deleteMax (IntMap Empty) = IntMap Empty
deleteMax m = delete (fst (findMax m)) m

-- | /O(min(n,W))/. Delete and find the minimal element.
deleteFindMin :: IntMap a -> ((Key, a), IntMap a)
deleteFindMin m = let (k, a) = findMin m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Delete and find the maximal element.
deleteFindMax :: IntMap a -> ((Key, a), IntMap a)
deleteFindMax m = let (k, a) = findMax m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: IntMap a -> Maybe (a, IntMap a)
minView (IntMap Empty) = Nothing
minView m = let (k, a) = findMin m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: IntMap a -> Maybe (a, IntMap a)
maxView (IntMap Empty) = Nothing
maxView m = let (k, a) = findMax m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing
minViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
minViewWithKey (IntMap Empty) = Nothing
minViewWithKey m = let (k, a) = findMin m
                   in Just ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing
maxViewWithKey :: IntMap a -> Maybe ((Key, a), IntMap a)
maxViewWithKey (IntMap Empty) = Nothing
maxViewWithKey m = let (k, a) = findMax m
                   in Just ((k, a), delete k m)

----------------------------

-- | Show the tree that implements the map.
showTree :: Show a => IntMap a -> String
showTree (IntMap m) = W.showTree m

valid :: IntMap a -> Bool
valid (IntMap m) = W.valid m

----------------------

i2w :: Int -> Word
i2w = fromIntegral

w2i :: Word -> Int
w2i = fromIntegral

-- | /O(1)/. Split a map into its negative and nonnegative parts. For internal use only.
{-# INLINE split0 #-}
split0 :: IntMap a -> (WordMap a, WordMap a)
split0 (IntMap Empty) = (Empty, Empty)
split0 (IntMap m@(NonEmpty min _ Tip))
    | w2i min < 0 = (m, Empty)
    | otherwise = (Empty, m)
split0 (IntMap m@(NonEmpty min minV (Bin max maxV l r)))
    | w2i (xor min max) < 0 = (W.flipBounds (NonEmpty max maxV r), NonEmpty min minV l)
    | w2i max < 0 = (m, Empty)
    | otherwise = (Empty, m)
