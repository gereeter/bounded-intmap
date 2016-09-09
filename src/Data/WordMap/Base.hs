{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordMap.Base where

import Control.DeepSeq (NFData(..))
import Control.Applicative (Applicative(..))

import Data.Monoid (Monoid(..))
import qualified Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))

import Data.Functor ((<$>))

import Data.Word (Word)
import Data.Bits (xor)

import Prelude hiding (foldr, foldl, lookup, null, map, min, max)

type Key = Word

newtype WordMap a = WordMap (WordMap_ a) deriving (Eq)
data WordMap_ a = NonEmpty {-# UNPACK #-} !Key a !(Node a) | Empty deriving (Eq)
data Node a = Bin {-# UNPACK #-} !Key a !(Node a) !(Node a) | Tip deriving (Eq, Show)

instance Show a => Show (WordMap a) where
    show m = "fromList " ++ show (toList m)

instance Functor WordMap where
    fmap f (WordMap m) = WordMap (fmap f m)

instance Functor WordMap_ where
    fmap _ Empty = Empty
    fmap f (NonEmpty min minV node) = NonEmpty min (f minV) (fmap f node)

instance Functor Node where
    fmap _ Tip = Tip
    fmap f (Bin k v l r) = Bin k (f v) (fmap f l) (fmap f r)

instance Data.Foldable.Foldable WordMap where
    foldMap f = start
      where
        start (WordMap Empty) = mempty
        start (WordMap (NonEmpty _ minV root)) = f minV `mappend` goL root
        
        goL Tip = mempty
        goL (Bin _ maxV l r) = goL l `mappend` goR r `mappend` f maxV
        
        goR Tip = mempty
        goR (Bin _ minV l r) = f minV `mappend` goL l `mappend` goR r
    
    foldr = foldr
    foldr' = foldr'
    foldl = foldl
    foldl' = foldl'

instance Traversable WordMap where
    traverse f = start
      where
        start (WordMap Empty) = pure (WordMap Empty)
        start (WordMap (NonEmpty min minV node)) = (\minV' root' -> WordMap (NonEmpty min minV' root')) <$> f minV <*> goL node
        
        goL Tip = pure Tip
        goL (Bin max maxV l r) = (\l' r' v' -> Bin max v' l' r') <$> goL l <*> goR r <*> f maxV
        
        goR Tip = pure Tip
        goR (Bin min minV l r) = Bin min <$> f minV <*> goL l <*> goR r

instance Monoid (WordMap a) where
    mempty = empty
    mappend = union

instance NFData a => NFData (WordMap a) where
    rnf (WordMap Empty) = ()
    rnf (WordMap (NonEmpty _ v n)) = rnf v `seq` rnf n

instance NFData a => NFData (Node a) where
    rnf Tip = ()
    rnf (Bin _ v l r) = rnf v `seq` rnf l `seq` rnf r

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'
(!) :: WordMap a -> Key -> a
(!) m k = findWithDefault (error $ "WordMap.!: key " ++ show k ++ " is not an element of the map") k m

-- | Same as 'difference'.
(\\) :: WordMap a -> WordMap b -> WordMap a
(\\) = difference

-- | /O(1)/. Is the map empty?
--
-- > Data.WordMap.null empty             == True
-- > Data.WordMap.null (singleton 1 'a') == False
null :: WordMap a -> Bool
null (WordMap Empty) = True
null _ = False

-- | /O(n)/. Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: WordMap a -> Int
size (WordMap Empty) = 0
size (WordMap (NonEmpty _ _ node)) = sizeNode node where
    sizeNode Tip = 1
    sizeNode (Bin _ _ l r) = sizeNode l + sizeNode r

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Key -> WordMap a -> Bool
member k = k `seq` start
  where
    start (WordMap Empty) = False
    start (WordMap (NonEmpty min _ node))
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) node
    
    goL !_ Tip = False
    goL !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = False
        | otherwise = True
      where xorCacheMax = xor k max
    
    goR !_ Tip = False
    goR !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = False
        | otherwise = True
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
notMember :: Key -> WordMap a -> Bool
notMember k = k `seq` start
  where
    start (WordMap Empty) = True
    start (WordMap (NonEmpty min _ node))
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) node
    
    goL !_ Tip = True
    goL !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = True
        | otherwise = False
      where xorCacheMax = xor k max
    
    goR !_ Tip = True
    goR !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = True
        | otherwise = False
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: Key -> WordMap a -> Maybe a
lookup k = k `seq` start
  where
    start (WordMap Empty) = Nothing
    start (WordMap (NonEmpty min minV node))
        | k < min = Nothing
        | k == min = Just minV
        | otherwise = goL (xor min k) node
    
    goL !_ Tip = Nothing
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = Nothing
        | otherwise = Just maxV
      where xorCacheMax = xor k max
    
    goR !_ Tip = Nothing
    goR !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = Nothing
        | otherwise = Just minV
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. The expression @'findWithDefault' def k map@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: a -> Key -> WordMap a -> a
findWithDefault def k = k `seq` start
  where
    start (WordMap Empty) = def
    start (WordMap (NonEmpty min minV node))
        | k < min = def
        | k == min = minV
        | otherwise = goL (xor min k) node
    
    goL !_ Tip = def
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = def
        | otherwise = maxV
      where xorCacheMax = xor k max
    
    goR !_ Tip = def
    goR !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = def
        | otherwise = minV
      where  xorCacheMin = xor min k

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Key -> WordMap a -> Maybe (Key, a)
lookupLT k = k `seq` start
  where
    start (WordMap Empty) = Nothing
    start (WordMap (NonEmpty min minV node))
        | min >= k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)
    
    goL !_ min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max < k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max
    
    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min >= k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k
    
    getMax min minV Tip = (min, minV)
    getMax _   _   (Bin max maxV _ _) = (max, maxV)

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Key -> WordMap a -> Maybe (Key, a)
lookupLE k = k `seq` start
  where
    start (WordMap Empty) = Nothing
    start (WordMap (NonEmpty min minV node))
        | min > k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)
    
    goL !_ min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max <= k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max
    
    goR !_ Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min > k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k
    
    getMax min minV Tip = (min, minV)
    getMax _   _   (Bin max maxV _ _) = (max, maxV)

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Key -> WordMap a -> Maybe (Key, a)
lookupGT k = k `seq` start
  where
    start (WordMap Empty) = Nothing
    start (WordMap (NonEmpty min minV Tip))
        | min <= k = Nothing
        | otherwise = Just (min, minV)
    start (WordMap (NonEmpty min minV (Bin max maxV l r)))
        | max <= k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))
    
    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max <= k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max
    
    goR !_ max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min > k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k
    
    getMin max maxV Tip = (max, maxV)
    getMin _   _   (Bin min minV _ _) = (min, minV)

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Key -> WordMap a -> Maybe (Key, a)
lookupGE k = k `seq` start
  where
    start (WordMap Empty) = Nothing
    start (WordMap (NonEmpty min minV Tip))
        | min < k = Nothing
        | otherwise = Just (min, minV)
    start (WordMap (NonEmpty min minV (Bin max maxV l r)))
        | max < k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))
    
    goL !_ Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max < k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max
    
    goR !_ max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min >= k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k
    
    getMin max maxV Tip = (max, maxV)
    getMin _   _   (Bin min minV _ _) = (min, minV)

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: WordMap a
empty = WordMap Empty

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> WordMap a -> WordMap a
delete k = k `seq` start
  where
    start (WordMap Empty) = WordMap Empty
    start m@(WordMap (NonEmpty min _ Tip))
        | k == min = WordMap Empty
        | otherwise = m
    start m@(WordMap (NonEmpty min minV root@(Bin max maxV l r)))
        | k < min = m
        | k == min = let DR min' minV' root' = deleteMinL max maxV l r in WordMap (NonEmpty min' minV' root')
        | otherwise = WordMap (NonEmpty min minV (deleteL k (xor min k) root))

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.WordMap)
data DeleteResult a = DR {-# UNPACK #-} !Key a !(Node a)

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: WordMap a -> WordMap a -> WordMap a
union = start
  where
    start (WordMap Empty) m2 = m2
    start m1 (WordMap Empty) = m1
    start (WordMap (NonEmpty min1 minV1 root1)) (WordMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = WordMap (NonEmpty min1 minV1 (goL2 minV2 min1 root1 min2 root2))
        | min1 > min2 = WordMap (NonEmpty min2 minV2 (goL1 minV1 min1 root1 min2 root2))
        | otherwise = WordMap (NonEmpty min1 minV1 (goLFused min1 root1 root2)) -- we choose min1 arbitrarily, as min1 == min2
    
    -- TODO: Should I bind 'minV1' in a closure? It never changes.
    -- TODO: Should I cache @xor min1 min2@?
    goL1 minV1 min1 Tip !_   Tip = Bin min1 minV1 Tip Tip
    goL1 minV1 min1 Tip min2 n2  = goInsertL1 min1 minV1 (xor min1 min2) min2 n2
    goL1 minV1 min1 n1  min2 Tip = insertMinL (xor min1 min2) min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 maxV2 l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
         LT | xor min2 max2 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min1 min2 < xor min1 max2 -> Bin max2 maxV2 (goL1 minV1 min1 n1 min2 l2) r2 -- we choose min1 arbitrarily - we just need something from tree 1
            | max1 > max2 -> Bin max1 maxV1 l2 (goR2 maxV2 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | max1 < max2 -> Bin max2 maxV2 l2 (goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2)
            | otherwise -> Bin max1 maxV1 l2 (goRFused max1 (Bin min1 minV1 l1 r1) r2) -- we choose max1 arbitrarily, as max1 == max2
         EQ | max2 < min1 -> disjoint
            | max1 > max2 -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goR2 maxV2 max1 r1 max2 r2)
            | max1 < max2 -> Bin max2 maxV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin max1 maxV1 (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
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
            | otherwise -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
         GT | xor min1 max1 `ltMSB` xor min1 min2 -> disjoint -- we choose min1 and min2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 minV2 min1 l1 min2 n2) r1 -- we choose min2 arbitrarily - we just need something from tree 2
            | max1 > max2 -> Bin max1 maxV1 l1 (goR2 maxV2 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | max1 < max2 -> Bin max2 maxV2 l1 (goR1 maxV1 max1 r1 max2 (Bin min2 minV2 l2 r2))
            | otherwise -> Bin max1 maxV1 l1 (goRFused max1 r1 (Bin min2 minV2 l2 r2)) -- we choose max1 arbitrarily, as max1 == max2
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
           | otherwise -> Bin max1 maxV1 (goLFused min l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
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
            | otherwise -> Bin min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2) r2 -- we choose min1 arbitrarily, as min1 == min2
         EQ | max1 < min2 -> disjoint
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2) -- we choose min1 arbitrarily, as min1 == min2
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
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goR2 maxV2 max1 r1 max2 r2) -- we choose min1 arbitrarily, as min1 == min2
         GT | xor min1 max1 `ltMSB` xor max1 max2 -> disjoint -- we choose max1 and max2 arbitrarily - we just need something from tree 1 and something from tree 2
            | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 maxV2 max1 r1 max2 n2) -- we choose max2 arbitrarily - we just need something from tree 2
            | min1 < min2 -> Bin min1 minV1 (goL2 minV2 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | min1 > min2 -> Bin min2 minV2 (goL1 minV1 min1 l1 min2 (Bin max2 maxV2 l2 r2)) r1
            | otherwise -> Bin min1 minV1 (goLFused min1 l1 (Bin max2 maxV2 l2 r2)) r1 -- we choose min1 arbitrarily, as min1 == min2
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
           | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (goRFused max r1 r2) -- we choose min1 arbitrarily, as min1 == min2
        GT -> Bin min1 minV1 l1 (goRFused max r1 n2)
    
    goInsertL1 k v !_        _    Tip = Bin k v Tip Tip
    goInsertL1 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL1 k v xorCache min l) r
                    else Bin max maxV l (goInsertR1 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max v l r
      where xorCacheMax = xor k max

    goInsertR1 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR1 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR1 k v xorCache max r)
                    else Bin min minV (goInsertL1 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min v l r
      where xorCacheMin = xor min k
    
    goInsertL2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertL2 k v !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goInsertL2 k v xorCache min l) r
                    else Bin max maxV l (goInsertR2 k v xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (insertMaxR xorCacheMax max maxV r)
        | otherwise = Bin max maxV l r
      where xorCacheMax = xor k max

    goInsertR2 k v !_        _    Tip = Bin k v Tip Tip
    goInsertR2 k v !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goInsertR2 k v xorCache max r)
                    else Bin min minV (goInsertL2 k v xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (insertMinL xorCacheMin min minV l) r
        | otherwise = Bin min minV l r
      where xorCacheMin = xor min k

-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: [WordMap a] -> WordMap a
unions = Data.Foldable.foldl' union empty

-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
difference :: WordMap a -> WordMap b -> WordMap a
difference = start
  where
    start (WordMap Empty) !_ = WordMap Empty
    start !m (WordMap Empty) = m
    start (WordMap (NonEmpty min1 minV1 root1)) (WordMap (NonEmpty min2 _ root2))
        | min1 < min2 = WordMap (NonEmpty min1 minV1 (goL2 min1 root1 min2 root2))
        | min1 > min2 = WordMap (goL1 minV1 min1 root1 min2 root2)
        | otherwise = WordMap (goLFused min1 root1 root2)
    
    goL1 minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL1 minV1 min1 n1 _ Tip = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = NonEmpty min1 minV1 n1
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> flipBounds $ NonEmpty max1 maxV1 (goR2 max1 (Bin min1 minV1 l1 r1) max2 r2)
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> flipBounds $ goRFused max1 (Bin min1 minV1 l1 r1) r2
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binL (goL1 minV1 min1 l1 min2 l2) (goRFused max1 r1 r2)
        GT -> binL (goL1 minV1 min1 l1 min2 n2) (NonEmpty max1 maxV1 r1)
    
    goL2 !_   Tip !_   !_  = Tip
    goL2 min1 n1  min2 Tip = deleteL min2 (xor min1 min2) n1
    goL2 _ n1@(Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = n1
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> Bin max1 maxV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
           | otherwise -> case goRFused max1 r1 r2 of
                Empty -> goL2 min1 l1 min2 l2
                NonEmpty max' maxV' r' -> Bin max' maxV' (goL2 min1 l1 min2 l2) r'
        GT | xor min1 min2 < xor min2 max1 -> Bin max1 maxV1 (goL2 min1 l1 min2 n2) r1 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> Bin max1 maxV1 l1 (goR2 max1 r1 max2 (Bin min2 dummyV l2 r2))
           | max1 < max2 -> case goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
           | otherwise -> case goRFused max1 r1 (Bin min2 dummyV l2 r2) of
                Empty -> l1
                NonEmpty max' maxV' r' -> Bin max' maxV' l1 r'
    
    goLFused min = loop
      where
        loop Tip !_ = Empty
        loop (Bin max1 maxV1 l1 r1) Tip = case deleteMinL max1 maxV1 l1 r1 of
            DR min' minV' n' -> NonEmpty min' minV' n'
        loop n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> loop n1 l2
            EQ | max1 > max2 -> binL (loop l1 l2) (NonEmpty max1 maxV1 (goR2 max1 r1 max2 r2))
               | max1 < max2 -> binL (loop l1 l2) (goR1 maxV1 max1 r1 max2 r2)
               | otherwise -> binL (loop l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
            GT -> binL (loop l1 n2) (NonEmpty max1 maxV1 r1)
    
    goR1 maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR1 maxV1 max1 n1 _ Tip = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = NonEmpty max1 maxV1 n1
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> flipBounds $ NonEmpty min1 minV1 (goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2)
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> flipBounds $ goLFused min1 (Bin max1 maxV1 l1 r1) l2
        EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> binR (goLFused min1 l1 l2) (goR1 maxV1 max1 r1 max2 r2)
        GT -> binR (NonEmpty min1 minV1 l1) (goR1 maxV1 max1 r1 max2 n2)
    
    goR2 !_   Tip !_   !_  = Tip
    goR2 max1 n1  max2 Tip = deleteR max2 (xor max1 max2) n1
    goR2 _ n1@(Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = n1
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2) 
           | otherwise -> case goLFused min1 l1 l2 of
                Empty -> goR2 max1 r1 max2 r2
                NonEmpty min' minV' l' -> Bin min' minV' l' (goR2 max1 r1 max2 r2) 
        GT | xor min1 max2 > xor max2 max1 -> Bin min1 minV1 l1 (goR2 max1 r1 max2 n2) -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> Bin min1 minV1 (goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)) r1
           | min1 > min2 -> case goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1
           | otherwise -> case goLFused min1 l1 (Bin max2 dummyV l2 r2) of
                Empty -> r1
                NonEmpty min' minV' l' -> Bin min' minV' l' r1
    
    goRFused max = loop
      where
        loop Tip !_ = Empty
        loop (Bin min1 minV1 l1 r1) Tip = case deleteMaxR min1 minV1 l1 r1 of
            DR max' maxV' n' -> NonEmpty max' maxV' n'
        loop n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> loop n1 r2
            EQ | min1 < min2 -> binR (NonEmpty min1 minV1 (goL2 min1 l1 min2 l2)) (loop r1 r2)
               | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (loop r1 r2)
               | otherwise -> binR (goLFused min1 r1 r2) (loop r1 r2) -- we choose min1 arbitrarily, as min1 == min2
            GT -> binR (NonEmpty min1 minV1 l1) (loop r1 n2)
    
    goLookupL k v !_ Tip = NonEmpty k v Tip
    goLookupL k v !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = NonEmpty k v Tip
        | otherwise = Empty
      where xorCacheMax = xor k max
    
    goLookupR k v !_ Tip = NonEmpty k v Tip
    goLookupR k v !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = NonEmpty k v Tip
        | otherwise = Empty
      where xorCacheMin = xor min k
    
    dummyV = error "impossible"

-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: WordMap a -> WordMap b -> WordMap a
intersection = start
  where
    start (WordMap Empty) !_ = WordMap Empty
    start !_ (WordMap Empty) = WordMap Empty
    start (WordMap (NonEmpty min1 minV1 root1)) (WordMap (NonEmpty min2 _ root2))
        | min1 < min2 = WordMap (goL2 min1 root1 min2 root2)
        | min1 > min2 = WordMap (goL1 minV1 min1 root1 min2 root2)
        | otherwise = WordMap (NonEmpty min1 minV1 (goLFused min1 root1 root2)) -- we choose min1 arbitrarily, as min1 == min2
    
    -- TODO: This scheme might produce lots of unnecessary flipBounds calls. This should be rectified.
    
    goL1 _     !_   !_  !_   Tip = Empty
    goL1 minV1 min1 Tip min2 n2  = goLookupL1 min1 minV1 (xor min1 min2) n2
    goL1 _ min1 (Bin _ _ _ _) _ (Bin max2 _ _ _) | min1 > max2 = Empty
    goL1 minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 min1 < xor min1 max2 -> goL1 minV1 min1 n1 min2 l2 -- min1 is arbitrary here - we just need something from tree 1
           | max1 > max2 -> flipBounds $ goR2 max1 (Bin min1 minV1 l1 r1) max2 r2
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
           | otherwise -> flipBounds $ NonEmpty max1 maxV1 (goRFused max1 (Bin min1 minV1 l1 r1) r2)
        EQ | max1 > max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL1 minV1 min1 l1 min2 l2 of
                Empty -> flipBounds (NonEmpty max1 maxV1 (goRFused max1 r1 r2))
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2))
        GT -> goL1 minV1 min1 l1 min2 n2
    
    goL2 !_   Tip !_   !_  = Empty
    goL2 min1 n1  min2 Tip = goLookupL2 min2 (xor min1 min2) n1
    goL2 _ (Bin max1 _ _ _) min2 (Bin _ _ _ _) | min2 > max1 = Empty
    goL2 min1 n1@(Bin max1 maxV1 l1 r1) min2 n2@(Bin max2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goL2 min1 n1 min2 l2
        EQ | max1 > max2 -> binL (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | max1 < max2 -> binL (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goL2 min1 l1 min2 l2 of
                Empty -> flipBounds (NonEmpty max1 maxV1 (goRFused max1 r1 r2))
                NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max1 maxV1 l' (goRFused max1 r1 r2))
        GT | xor min1 min2 < xor min2 max1 -> goL2 min1 l1 min2 n2 -- min2 is arbitrary here - we just need something from tree 2
           | max1 > max2 -> flipBounds $ goR2 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | max1 < max2 -> flipBounds $ goR1 maxV1 max1 r1 max2 (Bin min2 dummyV l2 r2)
           | otherwise -> flipBounds $ NonEmpty max1 maxV1 (goRFused max1 r1 (Bin min2 dummyV l2 r2))
    
    goLFused min = loop
      where
        loop Tip !_ = Tip
        loop !_ Tip = Tip
        loop n1@(Bin max1 maxV1 l1 r1) n2@(Bin max2 _ l2 r2) = case compareMSB (xor min max1) (xor min max2) of
            LT -> loop n1 l2
            EQ | max1 > max2 -> case goR2 max1 r1 max2 r2 of
                    Empty -> loop l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
               | max1 < max2 -> case goR1 maxV1 max1 r1 max2 r2 of
                    Empty -> loop l1 l2
                    NonEmpty max' maxV' r' -> Bin max' maxV' (loop l1 l2) r'
               | otherwise -> Bin max1 maxV1 (loop l1 l2) (goRFused max1 r1 r2) -- we choose max1 arbitrarily, as max1 == max2
            GT -> loop l1 n2
    
    goR1 _     !_   !_  !_   Tip = Empty
    goR1 maxV1 max1 Tip max2 n2  = goLookupR1 max1 maxV1 (xor max1 max2) n2
    goR1 _ max1 (Bin _ _ _ _) _ (Bin min2 _ _ _) | min2 > max1 = Empty
    goR1 maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT | xor min2 max1 > xor max1 max2 -> goR1 maxV1 max1 n1 max2 r2 -- max1 is arbitrary here - we just need something from tree 1
           | min1 < min2 -> flipBounds $ goL2 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
           | otherwise -> flipBounds $ NonEmpty min1 minV1 (goLFused min1 (Bin max1 maxV1 l1 r1) l2)
        EQ | min1 < min2 -> binR (goL2 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR1 maxV1 max1 r1 max2 r2)
           | otherwise -> case goR1 maxV1 max1 r1 max2 r2 of
                Empty -> flipBounds (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT -> goR1 maxV1 max1 r1 max2 n2
    
    goR2 !_   Tip !_   !_  = Empty
    goR2 max1 n1  max2 Tip = goLookupR2 max2 (xor max1 max2) n1
    goR2 _ (Bin min1 _ _ _) max2 (Bin _ _ _ _) | min1 > max2 = Empty
    goR2 max1 n1@(Bin min1 minV1 l1 r1) max2 n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max1) (xor min2 max2) of
        LT -> goR2 max1 n1 max2 r2
        EQ | min1 < min2 -> binR (goL2 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | min1 > min2 -> binR (goL1 minV1 min1 l1 min2 l2) (goR2 max1 r1 max2 r2)
           | otherwise -> case goR2 max1 r1 max2 r2 of
                Empty -> flipBounds (NonEmpty min1 minV1 (goLFused min1 l1 l2))
                NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min1 minV1 (goLFused min1 l1 l2) r')
        GT | xor min1 max2 > xor max2 max1 -> goR2 max1 r1 max2 n2 -- max2 is arbitrary here - we just need something from tree 2
           | min1 < min2 -> flipBounds $ goL2 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | min1 > min2 -> flipBounds $ goL1 minV1 min1 l1 min2 (Bin max2 dummyV l2 r2)
           | otherwise -> flipBounds $ NonEmpty min1 minV1 (goLFused min1 l1 (Bin max2 dummyV l2 r2))
    
    goRFused max = loop
      where
        loop Tip !_ = Tip
        loop !_ Tip = Tip
        loop n1@(Bin min1 minV1 l1 r1) n2@(Bin min2 _ l2 r2) = case compareMSB (xor min1 max) (xor min2 max) of
            LT -> loop n1 r2
            EQ | min1 < min2 -> case goL2 min1 l1 min2 l2 of
                    Empty -> loop r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
               | min1 > min2 -> case goL1 minV1 min1 l1 min2 l2 of
                    Empty -> loop r1 r2
                    NonEmpty min' minV' l' -> Bin min' minV' l' (loop r1 r2)
               | otherwise -> Bin min1 minV1 (goLFused min1 l1 l2) (loop r1 r2) -- we choose max1 arbitrarily, as max1 == max2
            GT -> loop r1 n2
    
    goLookupL1 !_ _ !_ Tip = Empty
    goLookupL1 k v !xorCache (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL1 k v xorCache l
                    else goLookupR1 k v xorCacheMax r
        | k > max = Empty
        | otherwise = NonEmpty k v Tip
      where xorCacheMax = xor k max
    
    goLookupR1 !_ _ !_ Tip = Empty
    goLookupR1 k v !xorCache (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR1 k v xorCache r
                    else goLookupL1 k v xorCacheMin l
        | k < min = Empty
        | otherwise = NonEmpty k v Tip
      where xorCacheMin = xor min k
    
    goLookupL2 !_ !_ Tip = Empty
    goLookupL2 k !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL2 k xorCache l
                    else goLookupR2 k xorCacheMax r
        | k > max = Empty
        | otherwise = NonEmpty k maxV Tip
      where xorCacheMax = xor k max
    
    goLookupR2 !_ !_ Tip = Empty
    goLookupR2 k !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR2 k xorCache r
                    else goLookupL2 k xorCacheMin l
        | k < min = Empty
        | otherwise = NonEmpty k minV Tip
      where xorCacheMin = xor min k
    
    dummyV = error "impossible"

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> WordMap a -> b
foldr f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty _ minV root)) = f minV (goL root z)
    
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
foldl :: (a -> b -> a) -> a -> WordMap b -> a
foldl f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty _ minV root)) = goL (f z minV) root
    
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
foldrWithKey :: (Key -> a -> b -> b) -> b -> WordMap a -> b
foldrWithKey f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty min minV root)) = f min minV (goL root z)
    
    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l (goR r (f max maxV acc))
    
    goR Tip acc = acc
    goR (Bin min minV l r) acc = f min minV (goL l (goR r acc))

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
foldlWithKey :: (a -> Key -> b -> a) -> a -> WordMap b -> a
foldlWithKey f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty min minV root)) = goL (f z min minV) root
    
    goL acc Tip = acc
    goL acc (Bin max maxV l r) = f (goR (goL acc l) r) max maxV
    
    goR acc Tip = acc
    goR acc (Bin min minV l r) = goR (goL (f acc min minV) l) r

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (Key -> a -> m) -> WordMap a -> m
foldMapWithKey f = start
  where
    start (WordMap Empty) = mempty
    start (WordMap (NonEmpty min minV root)) = f min minV `mappend` goL root
    
    goL Tip = mempty
    goL (Bin max maxV l r) = goL l `mappend` goR r `mappend` f max maxV
    
    goR Tip = mempty
    goR (Bin min minV l r) = f min minV `mappend` goL l `mappend` goR r

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> WordMap a -> b
foldr' f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty _ minV root)) = f minV $! goL root $! z
    
    goL Tip acc = acc
    goL (Bin _ maxV l r) acc = goL l $! goR r $! f maxV $! acc
    
    goR Tip acc = acc
    goR (Bin _ minV l r) acc = f minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> WordMap b -> a
foldl' f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty _ minV root)) = s goL (s f z minV) root
    
    goL acc Tip = acc
    goL acc (Bin _ maxV l r) = s f (s goR (s goL acc l) r) maxV
    
    goR acc Tip = acc
    goR acc (Bin _ minV l r) = s goR (s goL (s f acc minV) l) r
    
    s = ($!)

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (Key -> a -> b -> b) -> b -> WordMap a -> b
foldrWithKey' f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty min minV root)) = f min minV $! goL root $! z
    
    goL Tip acc = acc
    goL (Bin max maxV l r) acc = goL l $! goR r $! f max maxV $! acc
    
    goR Tip acc = acc
    goR (Bin min minV l r) acc = f min minV $! goL l $! goR r $! acc

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Key -> b -> a) -> a -> WordMap b -> a
foldlWithKey' f z = start
  where
    start (WordMap Empty) = z
    start (WordMap (NonEmpty min minV root)) = s goL (s f z min minV) root
    
    goL acc Tip = acc
    goL acc (Bin max maxV l r) = s f (s goR (s goL acc l) r) max maxV
    
    goR acc Tip = acc
    goR acc (Bin min minV l r) = s goR (s goL (s f acc min minV) l) r
    
    s = ($!)

-- TODO: make the conversion functions good producers

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
elems :: WordMap a -> [a]
elems = foldr (:) []

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
keys :: WordMap a -> [Key]
keys = foldrWithKey (\k _ l -> k : l) []

-- | /O(n)/. An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
assocs :: WordMap a -> [(Key, a)]
assocs = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: WordMap a -> [(Key, a)]
toList = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: WordMap a -> [(Key, a)]
toAscList = foldrWithKey (\k v l -> (k, v) : l) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]
toDescList :: WordMap a -> [(Key, a)]
toDescList = foldlWithKey (\l k v -> (k, v) : l) []

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
    start (WordMap Empty) = WordMap Empty
    start (WordMap (NonEmpty min minV root))
        | p min minV = WordMap (NonEmpty min minV (goL root))
        | otherwise = WordMap (goDeleteL root)
    
    goL Tip = Tip
    goL (Bin max maxV l r)
        | p max maxV = Bin max maxV (goL l) (goR r)
        | otherwise = case goDeleteR r of
            Empty -> goL l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goL l) r'
    
    goR Tip = Tip
    goR (Bin min minV l r)
        | p min minV = Bin min minV (goL l) (goR r)
        | otherwise = case goDeleteL l of
            Empty -> goR r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goR r)
    
    goDeleteL Tip = Empty
    goDeleteL (Bin max maxV l r)
        | p max maxV = case goDeleteL l of
            Empty -> case goR r of
                Tip -> NonEmpty max maxV Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV l' (goR r))
        | otherwise = binL (goDeleteL l) (goDeleteR r)
    
    goDeleteR Tip = Empty
    goDeleteR (Bin min minV l r)
        | p min minV = case goDeleteR r of
            Empty -> case goL l of
                Tip -> NonEmpty min minV Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV (goL l) r')
        | otherwise = binR (goDeleteL l) (goDeleteR r)

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
    start (WordMap Empty) = (WordMap Empty, WordMap Empty)
    start (WordMap (NonEmpty min minV root))
        | p min minV = let SP t f = goTrueL root
                       in (WordMap (NonEmpty min minV t), WordMap f)
        | otherwise  = let SP t f = goFalseL root
                       in (WordMap t, WordMap (NonEmpty min minV f))
    
    goTrueL Tip = SP Tip Empty
    goTrueL (Bin max maxV l r)
        | p max maxV = let SP tl fl = goTrueL l
                           SP tr fr = goTrueR r
                       in SP (Bin max maxV tl tr) (binL fl fr)
        | otherwise = let SP tl fl = goTrueL l
                          SP tr fr = goFalseR r
                          t = case tr of
                            Empty -> tl
                            NonEmpty max' maxV' r' -> Bin max' maxV' tl r'
                          f = case fl of
                            Empty -> flipBounds $ NonEmpty max maxV fr
                            NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max maxV l' fr)
                      in SP t f
    
    goTrueR Tip = SP Tip Empty
    goTrueR (Bin min minV l r)
        | p min minV = let SP tl fl = goTrueL l
                           SP tr fr = goTrueR r
                       in SP (Bin min minV tl tr) (binR fl fr)
        | otherwise = let SP tl fl = goFalseL l
                          SP tr fr = goTrueR r
                          t = case tl of
                            Empty -> tr
                            NonEmpty min' minV' l' -> Bin min' minV' l' tr
                          f = case fr of
                            Empty -> flipBounds $ NonEmpty min minV fl
                            NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min minV fl r')
                      in SP t f
    
    goFalseL Tip = SP Empty Tip
    goFalseL (Bin max maxV l r)
        | p max maxV = let SP tl fl = goFalseL l
                           SP tr fr = goTrueR r
                           t = case tl of
                             Empty -> flipBounds $ NonEmpty max maxV tr
                             NonEmpty min' minV' l' -> NonEmpty min' minV' (Bin max maxV l' tr)
                           f = case fr of
                             Empty -> fl
                             NonEmpty max' maxV' r' -> Bin max' maxV' fl r'
                       in SP t f
        | otherwise = let SP tl fl = goFalseL l
                          SP tr fr = goFalseR r
                      in SP (binL tl tr) (Bin max maxV fl fr)
    
    goFalseR Tip = SP Empty Tip
    goFalseR (Bin min minV l r)
        | p min minV = let SP tl fl = goTrueL l
                           SP tr fr = goFalseR r
                           t = case tr of
                             Empty -> flipBounds $ NonEmpty min minV tl
                             NonEmpty max' maxV' r' -> NonEmpty max' maxV' (Bin min minV tl r')
                           f = case fl of
                             Empty -> fr
                             NonEmpty min' minV' l' -> Bin min' minV' l' fr
                       in SP t f
        | otherwise = let SP tl fl = goFalseL l
                          SP tr fr = goFalseR r
                      in SP (binR tl tr) (Bin min minV fl fr)

data SP a b = SP !a !b

-- | /O(min(n,W))/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
split :: Key -> WordMap a -> (WordMap a, WordMap a)
split k m = case splitLookup k m of
    (lt, _, gt) -> (lt, gt)

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Key -> WordMap a -> (WordMap a, Maybe a, WordMap a)
splitLookup k = k `seq` start
  where
    start (WordMap Empty) = (WordMap Empty, Nothing, WordMap Empty)
    start m@(WordMap (NonEmpty min minV root))
        | k > min = case root of
            Tip -> (m, Nothing, WordMap Empty)
            Bin max maxV l r | k < max -> let (DR glb glbV lt, eq, DR lub lubV gt) = go (xor min k) min minV (xor k max) max maxV l r
                                          in (WordMap (flipBounds (NonEmpty glb glbV lt)), eq, WordMap (NonEmpty lub lubV gt))
                             | k > max -> (m, Nothing, WordMap Empty)
                             | otherwise -> let DR max' maxV' root' = deleteMaxR min minV l r
                                            in (WordMap (flipBounds (NonEmpty max' maxV' root')), Just maxV, WordMap Empty)

        | k < min = (WordMap Empty, Nothing, m)
        | otherwise = case root of
            Tip -> (WordMap Empty, Just minV, WordMap Empty)
            Bin max maxV l r -> let DR min' minV' root' = deleteMinL max maxV l r
                                in (WordMap Empty, Just minV, WordMap (NonEmpty min' minV' root'))
    
    go xorCacheMin min minV xorCacheMax max maxV l r
        | xorCacheMin < xorCacheMax = case l of
            Tip -> (DR min minV Tip, Nothing, flipBoundsDR (DR max maxV r))
            Bin maxI maxVI lI rI
                | k < maxI -> let (lt, eq, DR minI minVI gt) = go xorCacheMin min minV (xor k maxI) maxI maxVI lI rI
                              in (lt, eq, DR minI minVI (Bin max maxV gt r))
                | k > maxI -> (flipBoundsDR (DR min minV l), Nothing, flipBoundsDR (DR max maxV r))
                | otherwise -> (deleteMaxR min minV lI rI, Just maxVI, flipBoundsDR (DR max maxV r))
        | otherwise = case r of
            Tip -> (flipBoundsDR (DR min minV l), Nothing, DR max maxV Tip)
            Bin minI minVI lI rI
                | k > minI -> let (DR maxI maxVI lt, eq, gt) = go (xor minI k) minI minVI xorCacheMax max maxV lI rI
                              in (DR maxI maxVI (Bin min minV l lt), eq, gt)
                | k < minI -> (flipBoundsDR (DR min minV l), Nothing, flipBoundsDR (DR max maxV r))
                | otherwise -> (flipBoundsDR (DR min minV l), Just minVI, deleteMinL max maxV lI rI)

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
splitRoot :: WordMap a -> [WordMap a]
splitRoot (WordMap Empty) = []
splitRoot m@(WordMap (NonEmpty _ _ Tip)) = [m]
splitRoot (WordMap (NonEmpty min minV (Bin max maxV l r))) = [WordMap (NonEmpty min minV l), WordMap (flipBounds (NonEmpty max maxV r))]

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => WordMap a -> WordMap a -> Bool
isSubmapOf = isSubmapOfBy (==)

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
isSubmapOfBy :: (a -> b -> Bool) -> WordMap a -> WordMap b -> Bool
isSubmapOfBy p = start
  where
    start (WordMap Empty) !_ = True
    start !_ (WordMap Empty) = False
    start (WordMap (NonEmpty min1 minV1 root1)) (WordMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = False
        | min1 > min2 = goL minV1 min1 root1 min2 root2
        | otherwise = p minV1 minV2 && goLFused min1 root1 root2
    
    goL minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 min1 < xor min1 max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goRFused max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFused max1 r1 r2 -- EQ
    
    goLFused _ Tip _ = True
    goLFused _ _ Tip = False
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min max1 `ltMSB` xor min max2 of
            True -> goLFused min n1 l2
            False -> goLFused min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFused min l1 l2 && goRFused max1 r1 r2
    
    goR maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 max1 > xor max1 max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goLFused min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFused min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
    
    goRFused _ Tip _ = True
    goRFused _ _ Tip = False
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max `ltMSB` xor min2 max of
            True -> goRFused max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFused max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFused min1 l1 l2 && goRFused max r1 r2
    
    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max
    
    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = False
        | otherwise = p v minV
      where  xorCacheMin = xor min k

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => WordMap a -> WordMap a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

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
isProperSubmapOfBy :: (a -> b -> Bool) -> WordMap a -> WordMap b -> Bool
isProperSubmapOfBy p m1 m2 = submapCmp p m1 m2 == LT

submapCmp :: (a -> b -> Bool) -> WordMap a -> WordMap b -> Ordering
submapCmp p = start
  where
    start (WordMap Empty) (WordMap Empty) = EQ
    start (WordMap Empty) !_ = LT
    start !_ (WordMap Empty) = GT
    start (WordMap (NonEmpty min1 minV1 root1)) (WordMap (NonEmpty min2 minV2 root2))
        | min1 < min2 = GT
        | min1 > min2 = fromBool $ goL minV1 min1 root1 min2 root2
        | p minV1 minV2 = goLFused min1 root1 root2
        | otherwise = GT
    
    goL minV1 min1 Tip min2 n2 = goLookupL min1 minV1 (xor min1 min2) n2
    goL _     _    _   _    Tip = False
    goL minV1 min1 n1@(Bin max1 maxV1 l1 r1) min2 (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 min1 < xor min1 max2 -> goL minV1 min1 n1 min2 l2 -- LT
                 | otherwise -> goR maxV1 max1 (Bin min1 minV1 l1 r1) max2 r2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goRFusedBool max1 (Bin min1 minV1 l1 r1) r2 -- LT
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max1 r1 r2 -- EQ
    
    goLFused _ Tip Tip = EQ
    goLFused _ Tip _ = LT
    goLFused _ _ Tip = GT
    goLFused min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = GT
        | max1 < max2 = fromBool $ case xor min max1 `ltMSB` xor min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | p maxV1 maxV2 = goLFused min l1 l2 `combine` goRFused max1 r1 r2
        | otherwise = GT
    
    goLFusedBool _ Tip _ = True
    goLFusedBool _ _ Tip = False
    goLFusedBool min n1@(Bin max1 maxV1 l1 r1) (Bin max2 maxV2 l2 r2)
        | max1 > max2 = False
        | max1 < max2 = case xor min max1 `ltMSB` xor min max2 of
            True -> goLFusedBool min n1 l2
            False -> goLFusedBool min l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p maxV1 maxV2 && goLFusedBool min l1 l2 && goRFusedBool max1 r1 r2
    
    goR maxV1 max1 Tip max2 n2 = goLookupR max1 maxV1 (xor max1 max2) n2
    goR _     _    _   _    Tip = False
    goR maxV1 max1 n1@(Bin min1 minV1 l1 r1) max2 (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max1 `ltMSB` xor min2 max2 of
            True | xor min2 max1 > xor max1 max2 -> goR maxV1 max1 n1 max2 r2 -- LT
                 | otherwise -> goL minV1 min1 (Bin max1 maxV1 l1 r1) min2 l2
            False -> goL minV1 min1 l1 min2 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
        | otherwise = p minV1 minV2 && case xor min1 max1 `ltMSB` xor min2 max1 of
            True -> goLFusedBool min1 (Bin max1 maxV1 l1 r1) l2 -- LT
            False -> goLFusedBool min1 l1 l2 && goR maxV1 max1 r1 max2 r2 -- EQ
    
    goRFused _ Tip Tip = EQ
    goRFused _ Tip _ = LT
    goRFused _ _ Tip = GT
    goRFused max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = GT
        | min1 > min2 = fromBool $ case xor min1 max `ltMSB` xor min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | p minV1 minV2 = goLFused min1 l1 l2 `combine` goRFused max r1 r2
        | otherwise = GT
    
    goRFusedBool _ Tip _ = True
    goRFusedBool _ _ Tip = False
    goRFusedBool max n1@(Bin min1 minV1 l1 r1) (Bin min2 minV2 l2 r2)
        | min1 < min2 = False
        | min1 > min2 = case xor min1 max `ltMSB` xor min2 max of
            True -> goRFusedBool max n1 r2
            False -> goL minV1 min1 l1 min2 l2 && goRFusedBool max r1 r2 -- EQ
        | otherwise = p minV1 minV2 && goLFusedBool min1 l1 l2 && goRFusedBool max r1 r2
    
    goLookupL _ _ !_ Tip = False
    goLookupL k v !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goLookupL k v xorCache l
                    else goLookupR k v xorCacheMax r
        | k > max = False
        | otherwise = p v maxV
      where xorCacheMax = xor k max
    
    goLookupR _ _ !_ Tip = False
    goLookupR k v !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goLookupR k v xorCache r
                    else goLookupL k v xorCacheMin l
        | k < min = False
        | otherwise = p v minV
      where  xorCacheMin = xor min k
    
    fromBool True = LT
    fromBool False = GT
    
    combine GT _ = GT
    combine _ GT = GT
    combine EQ EQ = EQ
    combine _ _ = LT

-- | /O(1)/. The minimal key of the map.
findMin :: WordMap a -> (Key, a)
findMin (WordMap Empty) = error "findMin: empty map has no minimal element"
findMin (WordMap (NonEmpty min minV _)) = (min, minV)

-- | /O(1)/. The maximal key of the map.
findMax :: WordMap a -> (Key, a)
findMax (WordMap Empty) = error "findMin: empty map has no minimal element"
findMax (WordMap (NonEmpty min minV root)) = case root of
    Tip -> (min, minV)
    Bin max maxV _ _ -> (max, maxV)

-- | /O(min(n,W))/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMin :: WordMap a -> WordMap a
deleteMin (WordMap Empty) = WordMap Empty
deleteMin m = delete (fst (findMin m)) m

-- | /O(min(n,W))/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'IntMap' was already empty.
deleteMax :: WordMap a -> WordMap a
deleteMax (WordMap Empty) = WordMap Empty
deleteMax m = delete (fst (findMax m)) m

-- | /O(min(n,W))/. Delete and find the minimal element.
deleteFindMin :: WordMap a -> ((Key, a), WordMap a)
deleteFindMin m = let (k, a) = findMin m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Delete and find the maximal element.
deleteFindMax :: WordMap a -> ((Key, a), WordMap a)
deleteFindMax m = let (k, a) = findMax m
                  in ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: WordMap a -> Maybe (a, WordMap a)
minView (WordMap Empty) = Nothing
minView m = let (k, a) = findMin m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: WordMap a -> Maybe (a, WordMap a)
maxView (WordMap Empty) = Nothing
maxView m = let (k, a) = findMax m
            in Just (a, delete k m)

-- | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing
minViewWithKey :: WordMap a -> Maybe ((Key, a), WordMap a)
minViewWithKey (WordMap Empty) = Nothing
minViewWithKey m = let (k, a) = findMin m
                   in Just ((k, a), delete k m)

-- | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing
maxViewWithKey :: WordMap a -> Maybe ((Key, a), WordMap a)
maxViewWithKey (WordMap Empty) = Nothing
maxViewWithKey m = let (k, a) = findMax m
                   in Just ((k, a), delete k m)

----------------------------

-- | Show the tree that implements the map.
showTree :: Show a => WordMap a -> String
showTree = unlines . aux where
    aux (WordMap Empty) = []
    aux (WordMap (NonEmpty min minV node)) = (show min ++ " " ++ show minV) : auxNode False node
    auxNode _ Tip = ["+-."]
    auxNode lined (Bin bound val l r) = ["+--" ++ show bound ++ " " ++ show val, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent line = prefix : "  " ++ line

valid :: WordMap a -> Bool
valid = start
  where
    start (WordMap Empty) = True
    start (WordMap (NonEmpty min _ root)) = allKeys (> min) root && goL min root
    
    goL _    Tip = True
    goL min (Bin max _ l r) =
           allKeys (< max) l
        && allKeys (< max) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    goR _    Tip = True
    goR max (Bin min _ l r) =
           allKeys (> min) l
        && allKeys (> min) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    allKeys _ Tip = True
    allKeys p (Bin b _ l r) = p b && allKeys p l && allKeys p r

-- | /O(1)/. Returns whether the most significant bit of its first
-- argument is less significant than the most significant bit of its
-- second argument.
{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < xor x y

{-# INLINE compareMSB #-}
compareMSB :: Word -> Word -> Ordering
compareMSB x y = case compare x y of
    LT | x < xor x y -> LT
    GT | y < xor x y -> GT
    _ -> EQ

{-# INLINE binL #-}
binL :: WordMap_ a -> WordMap_ a -> WordMap_ a
binL Empty r = flipBounds r
binL l Empty = l
binL (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty min minV (Bin max maxV l r)

{-# INLINE binR #-}
binR :: WordMap_ a -> WordMap_ a -> WordMap_ a
binR Empty r = r
binR l Empty = flipBounds l
binR (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty max maxV (Bin min minV l r)

{-# INLINE flipBounds #-}
flipBounds :: WordMap_ a -> WordMap_ a
flipBounds Empty = Empty
flipBounds n@(NonEmpty _ _ Tip) = n
flipBounds (NonEmpty b1 v1 (Bin b2 v2 l r)) = NonEmpty b2 v2 (Bin b1 v1 l r)

{-# INLINE flipBoundsDR #-}
flipBoundsDR :: DeleteResult a -> DeleteResult a
flipBoundsDR n@(DR _ _ Tip) = n
flipBoundsDR (DR b1 v1 (Bin b2 v2 l r)) = DR b2 v2 (Bin b1 v1 l r)

-- | Insert a key/value pair to a left node where the key is smaller than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately prior to it (the minimum bound of the node).
insertMinL :: Word -> Key -> a -> Node a -> Node a
insertMinL !_ !min minV Tip = Bin min minV Tip Tip
insertMinL !xorCache !min minV (Bin max maxV l r)
    | xor min max < xorCache = Bin max maxV Tip (Bin min minV l r)
    | otherwise = Bin max maxV (insertMinL xorCache min minV l) r

-- | Insert a key/value pair to a right node where the key is greater than
-- any present in that node. Requires the xor of the inserted key and the
-- key immediately following it (the maximum bound of the node).
insertMaxR :: Word -> Key -> a -> Node a -> Node a
insertMaxR !_ !max maxV Tip = Bin max maxV Tip Tip
insertMaxR !xorCache !max maxV (Bin min minV l r)
    | xor min max < xorCache = Bin min minV (Bin max maxV l r) Tip
    | otherwise = Bin min minV l (insertMaxR xorCache max maxV r)

-- | Delete the minimum key/value pair from an unpacked left node, returning
-- a new left node in a DeleteResult.
deleteMinL :: Key -> a -> Node a -> Node a -> DeleteResult a
deleteMinL !max maxV Tip Tip = DR max maxV Tip
deleteMinL !max maxV Tip (Bin min minV l r) = DR min minV (Bin max maxV l r)
deleteMinL !max maxV (Bin innerMax innerMaxV innerL innerR) r =
    let DR min minV inner = deleteMinL innerMax innerMaxV innerL innerR
    in  DR min minV (Bin max maxV inner r)

-- | Delete the maximum key/value pair from an unpacked right node, returning
-- a new right node in a DeleteResult.
deleteMaxR :: Key -> a -> Node a -> Node a -> DeleteResult a
deleteMaxR !min minV Tip Tip = DR min minV Tip
deleteMaxR !min minV (Bin max maxV l r) Tip = DR max maxV (Bin min minV l r)
deleteMaxR !min minV l (Bin innerMin innerMinV innerL innerR) =
    let DR max maxV inner = deleteMaxR innerMin innerMinV innerL innerR
    in  DR max maxV (Bin min minV l inner)

-- | Combine two disjoint nodes into a new left node. This is not cheap.
extractBinL :: Node a -> Node a -> Node a
extractBinL l Tip = l
extractBinL l (Bin min minV innerL innerR) =
    let DR max maxV r = deleteMaxR min minV innerL innerR
    in Bin max maxV l r

-- | Combine two disjoint nodes into a new right node. This is not cheap.
extractBinR :: Node a -> Node a -> Node a
extractBinR Tip r = r
extractBinR (Bin max maxV innerL innerR) r =
    let DR min minV l = deleteMinL max maxV innerL innerR
    in Bin min minV l r

nodeToMapL :: Node a -> WordMap_ a
nodeToMapL Tip = Empty
nodeToMapL (Bin max maxV innerL innerR) =
    let DR min minV l = deleteMinL max maxV innerL innerR
    in NonEmpty min minV l

nodeToMapR :: Node a -> WordMap_ a
nodeToMapR Tip = Empty
nodeToMapR (Bin min minV innerL innerR) =
    let DR max maxV r = deleteMaxR min minV innerL innerR
    in NonEmpty max maxV r

-- | Delete a key from a left node. Takes the xor of the deleted key and
-- the minimum bound of that node.
deleteL :: Key -> Word -> Node a -> Node a
deleteL !_ !_ Tip = Tip
deleteL !k !xorCache n@(Bin max maxV l r)
    | k < max = if xorCache < xorCacheMax
                then Bin max maxV (deleteL k xorCache l) r
                else Bin max maxV l (deleteR k xorCacheMax r)
    | k > max = n
    | otherwise = extractBinL l r
  where xorCacheMax = xor k max

-- | Delete a key from a right node. Takes the xor of the deleted key and
-- the maximum bound of that node.
deleteR :: Key -> Word -> Node a -> Node a
deleteR !_ !_ Tip = Tip
deleteR !k !xorCache n@(Bin min minV l r)
    | k > min = if xorCache < xorCacheMin
                then Bin min minV l (deleteR k xorCache r)
                else Bin min minV (deleteL k xorCacheMin l) r
    | k < min = n
    | otherwise = extractBinR l r
  where xorCacheMin = xor min k
