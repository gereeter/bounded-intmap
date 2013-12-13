{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordMap.Internal where

import Control.DeepSeq
import Control.Applicative hiding (empty)

import Data.Monoid
import qualified Data.Foldable (Foldable(..))
import Data.Traversable

import Data.Word (Word)
import Data.Bits (xor, (.|.))

import Prelude hiding (foldr, foldl, lookup, null, map)

type Key = Word

data WordMap a = NonEmpty {-# UNPACK #-} !Key a !(Node a) | Empty deriving (Eq)
data Node a = Bin {-# UNPACK #-} !Key a !(Node a) !(Node a) | Tip deriving (Eq, Show)

instance Show a => Show (WordMap a) where
    show m = "fromList " ++ show (toList m)

instance Functor WordMap where
    fmap f Empty = Empty
    fmap f (NonEmpty min minV node) = NonEmpty min (f minV) (fmap f node)

instance Functor Node where
    fmap f Tip = Tip
    fmap f (Bin k v l r) = Bin k (f v) (fmap f l) (fmap f r)

instance Data.Foldable.Foldable WordMap where
    foldMap f Empty = mempty
    foldMap f (NonEmpty _ v node) = f v `mappend` goL node
      where
        goL Tip = mempty
        goL (Bin _ v l r) = goL l `mappend` goR r `mappend` f v
        
        goR Tip = mempty
        goR (Bin _ v l r) = f v `mappend` goL l `mappend` goR r
    
    foldr = foldr
    foldr' = foldr'
    foldl = foldl
    foldl' = foldl'

instance Traversable WordMap where
    traverse f Empty = pure Empty
    traverse f (NonEmpty min minV node) = NonEmpty min <$> f minV <*> goL node
      where
        goL Tip = pure Tip
        goL (Bin max maxV l r) = (\l' r' v' -> Bin max v' l' r') <$> goL l <*> goR r <*> f maxV
        
        goR Tip = pure Tip
        goR (Bin min minV l r) = Bin min <$> f minV <*> goL l <*> goR r
{-
instance Monoid (WordMap a) where
    mempty = empty
    mappend = union
-}
instance NFData a => NFData (WordMap a) where
    rnf Empty = ()
    rnf (NonEmpty _ v n) = rnf v `seq` rnf n

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

-- | /O(1)/. Is the map empty?
null :: WordMap a -> Bool
null Empty = True
null _ = False

-- | /O(n)/. Number of elements in the map.
size :: WordMap a -> Int
size Empty = 0
size (NonEmpty _ _ node) = sizeNode node where
    sizeNode Tip = 1
    sizeNode (Bin _ _ l r) = sizeNode l + sizeNode r

-- | /O(1)/. Find the smallest and largest key in the map.
bounds :: WordMap a -> Maybe (Key, Key)
bounds Empty = Nothing
bounds (NonEmpty min _ Tip) = Just (min, min)
bounds (NonEmpty min _ (Bin max _ _ _)) = Just (min, max)

-- TODO: Is there a good way to unify the 'lookup'-like functions?

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> WordMap a -> Bool
member k = k `seq` start
  where
    start Empty = False
    start (NonEmpty min _ node)
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min Tip = False
    goL !xorCache min (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache min l
                    else goR xorCacheMax max r
        | k > max = False
        | otherwise = True
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = False
    goR !xorCache max (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache max r
                    else goL xorCacheMin min l
        | k < min = False
        | otherwise = True
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Is the key not a member of the map?
notMember :: Key -> WordMap a -> Bool
notMember k = k `seq` start
  where
    start Empty = True
    start (NonEmpty min _ node)
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min Tip = True
    goL !xorCache min (Bin max _ l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache min l
                    else goR xorCacheMax max r
        | k > max = True
        | otherwise = False
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = True
    goR !xorCache max (Bin min _ l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache max r
                    else goL xorCacheMin min l
        | k < min = True
        | otherwise = False
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Key -> WordMap a -> Maybe a
lookup k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min minV node)
        | k < min = Nothing
        | k == min = Just minV
        | otherwise = goL (xor min k) node
    
    goL !xorCache Tip = Nothing
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = Nothing
        | otherwise = Just maxV
      where xorCacheMax = xor k max
    
    goR !xorCache Tip = Nothing
    goR !xorCache (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | k < min = Nothing
        | otherwise = Just minV
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. The expression @findWithDefault def k map@ returns
-- the value at key @k@ or returns @def@ when the key is not an element
-- of the map. 
findWithDefault :: a -> Key -> WordMap a -> a
findWithDefault def k = k `seq` start
  where
    start Empty = def
    start (NonEmpty min minV node)
        | k < min = def
        | k == min = minV
        | otherwise = goL (xor min k) node
    
    goL !xorCache Tip = def
    goL !xorCache (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | k > max = def
        | otherwise = maxV
      where xorCacheMax = xor k max
    
    goR !xorCache Tip = def
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
    start Empty = Nothing
    start (NonEmpty min minV node)
        | min >= k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)
    
    goL !xorCache min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max < k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max
    
    goR !xorCache Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min >= k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k
    
    getMax min minV Tip = (min, minV)
    getMax min minV (Bin max maxV l r) = (max, maxV)

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
    start (NonEmpty min minV node)
        | min > k = Nothing
        | otherwise = Just (goL (xor min k) min minV node)
    
    goL !xorCache min minV Tip = (min, minV)
    goL !xorCache min minV (Bin max maxV l r)
        | max <= k = (max, maxV)
        | xorCache < xorCacheMax = goL xorCache min minV l
        | otherwise = goR xorCacheMax r min minV l
      where
        xorCacheMax = xor k max
    
    goR !xorCache Tip fMin fMinV fallback = getMax fMin fMinV fallback
    goR !xorCache (Bin min minV l r) fMin fMinV fallback
        | min > k = getMax fMin fMinV fallback
        | xorCache < xorCacheMin = goR xorCache r min minV l
        | otherwise = goL xorCacheMin min minV l
      where
        xorCacheMin = xor min k
    
    getMax min minV Tip = (min, minV)
    getMax min minV (Bin max maxV l r) = (max, maxV)

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Key -> WordMap a -> Maybe (Key, a)
lookupGT k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min minV Tip)
        | min <= k = Nothing
        | otherwise = Just (min, minV)
    start (NonEmpty min minV (Bin max maxV l r))
        | max <= k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))
    
    goL !xorCache Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max <= k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max
    
    goR !xorCache max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min > k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k
    
    getMin max maxV Tip = (max, maxV)
    getMin max maxV (Bin min minV _ _) = (min, minV)

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
    start (NonEmpty min minV Tip)
        | min < k = Nothing
        | otherwise = Just (min, minV)
    start (NonEmpty min minV (Bin max maxV l r))
        | max < k = Nothing
        | otherwise = Just (goR (xor k max) max maxV (Bin min minV l r))
    
    goL !xorCache Tip fMax fMaxV fallback = getMin fMax fMaxV fallback
    goL !xorCache (Bin max maxV l r) fMax fMaxV fallback
        | max < k = getMin fMax fMaxV fallback
        | xorCache < xorCacheMax = goL xorCache l max maxV r
        | otherwise = goR xorCacheMax max maxV r
      where
        xorCacheMax = xor k max
    
    goR !xorCache max maxV Tip = (max, maxV)
    goR !xorCache max maxV (Bin min minV l r)
        | min >= k = (min, minV)
        | xorCache < xorCacheMin = goR xorCache max maxV r
        | otherwise = goL xorCacheMin l max maxV r
      where
        xorCacheMin = xor min k
    
    getMin max maxV Tip = (max, maxV)
    getMin max maxV (Bin min minV _ _) = (min, minV)

-- | /O(1)/. The empty map.
empty :: WordMap a
empty = Empty

-- | /O(1)/. A map of one element.
singleton :: Key -> a -> WordMap a
singleton k v = NonEmpty k v Tip

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value
-- is replaced with the supplied value. 
insert :: Key -> a -> WordMap a -> WordMap a
insert k v = k `seq` start
  where
    start Empty = NonEmpty k v Tip
    start (NonEmpty min minV root)
        | k > min = NonEmpty min minV (goL (xor min k) min root)
        | k < min = NonEmpty k v (endL (xor min k) min minV root)
        | otherwise = NonEmpty k v root
    
    goL !xorCache min Tip = Bin k v Tip Tip
    goL !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (endR xorCacheMax max maxV r)
        | otherwise = Bin max v l r
      where xorCacheMax = xor k max

    goR !xorCache max Tip = Bin k v Tip Tip
    goR !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (endL xorCacheMin min minV l) r
        | otherwise = Bin min v l r
      where xorCacheMin = xor min k
    
    endL !xorCache min minV = go
      where
        go Tip = Bin min minV Tip Tip
        go (Bin max maxV l r)
            | xor min max < xorCache = Bin max maxV Tip (Bin min minV l r)
            | otherwise = Bin max maxV (go l) r

    endR !xorCache max maxV = go
      where
        go Tip = Bin max maxV Tip Tip
        go (Bin min minV l r)
            | xor min max < xorCache = Bin min minV (Bin max maxV l r) Tip
            | otherwise = Bin min minV l (go r)

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
insertWith combine k v = k `seq` start
  where
    start Empty = NonEmpty k v Tip
    start (NonEmpty min minV root)
        | k > min = NonEmpty min minV (goL (xor min k) min root)
        | k < min = NonEmpty k v (endL (xor min k) min minV root)
        | otherwise = NonEmpty k (combine v minV) root
    
    goL !xorCache min Tip = Bin k v Tip Tip
    goL !xorCache min (Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k v (Bin max maxV l r) Tip
                    else Bin k v l (endR xorCacheMax max maxV r)
        | otherwise = Bin max (combine v maxV) l r
      where xorCacheMax = xor k max

    goR !xorCache max Tip = Bin k v Tip Tip
    goR !xorCache max (Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = if xor min max < xorCacheMin
                    then Bin k v Tip (Bin min minV l r)
                    else Bin k v (endL xorCacheMin min minV l) r
        | otherwise = Bin min (combine v minV) l r
      where xorCacheMin = xor min k
    
    endL !xorCache min minV = finishL
      where
        finishL Tip = Bin min minV Tip Tip
        finishL (Bin max maxV l r)
            | xor min max < xorCache = Bin max maxV Tip (Bin min minV l r)
            | otherwise = Bin max maxV (finishL l) r

    endR !xorCache max maxV = finishR
      where
        finishR Tip = Bin max maxV Tip Tip
        finishR (Bin min minV l r)
            | xor min max < xorCache = Bin min minV (Bin max maxV l r) Tip
            | otherwise = Bin min minV l (finishR r)

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

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> WordMap a -> WordMap a
delete k = k `seq` start
  where
    start Empty = Empty
    start m@(NonEmpty min _ Tip)
        | k == min = Empty
        | otherwise = m
    start m@(NonEmpty min minV root@(Bin max maxV l r))
        | k < min = m
        | k == min = let DR min' minV' root' = goDeleteMin max maxV l r in NonEmpty min' minV' root'
        | otherwise = NonEmpty min minV (goL (xor min k) min root)
    
    goL !xorCache min Tip = Tip
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case r of
            Tip -> l
            Bin minI minVI lI rI -> let DR max' maxV' r' = goDeleteMax minI minVI lI rI
                                    in  Bin max' maxV' l r'
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = Tip
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case l of
            Tip -> r
            Bin maxI maxVI lI rI -> let DR min' minV' l' = goDeleteMin maxI maxVI lI rI
                                    in  Bin min' minV' l' r
      where xorCacheMin = xor min k
    
    goDeleteMin max maxV l r = case l of
        Tip -> case r of
            Tip -> DR max maxV r
            Bin min minV l' r' -> DR min minV (Bin max maxV l' r')
        Bin maxI maxVI lI rI -> let DR min minV l' = goDeleteMin maxI maxVI lI rI
                                in  DR min minV (Bin max maxV l' r)
    
    goDeleteMax min minV l r = case r of
        Tip -> case l of
            Tip -> DR min minV l
            Bin max maxV l' r' -> DR max maxV (Bin min minV l' r')
        Bin minI minVI lI rI -> let DR max maxV r' = goDeleteMax minI minVI lI rI
                                in  DR max maxV (Bin min minV l r')

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.WordMap)
data DeleteResult a = DR {-# UNPACK #-} !Key a !(Node a)

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
        | otherwise = NonEmpty min (f minV) node
    
    goL !xorCache min Tip = Tip
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = Bin max (f maxV) l r
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = Tip
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = Bin min (f minV) l r
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
            Just minV' -> NonEmpty min minV' Tip
        | otherwise = m
    start m@(NonEmpty min minV root@(Bin max maxV l r))
        | k < min = m
        | k == min = case f minV of
            Nothing -> let DR min' minV' root' = goDeleteMin max maxV l r
                       in NonEmpty min' minV' root'
            Just minV' -> NonEmpty min minV' root
        | otherwise = NonEmpty min minV (goL (xor min k) min root)
    
    goL !xorCache min Tip = Tip
    goL !xorCache min n@(Bin max maxV l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max maxV (goL xorCache min l) r
                    else Bin max maxV l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case f maxV of
            Nothing -> case r of
                Tip -> l
                Bin minI minVI lI rI -> let DR max' maxV' r' = goDeleteMax minI minVI lI rI
                                        in  Bin max' maxV' l r'
            Just maxV' -> Bin max maxV' l r
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = Tip
    goR !xorCache max n@(Bin min minV l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min minV l (goR xorCache max r)
                    else Bin min minV (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case f minV of
            Nothing -> case l of
                Tip -> r
                Bin maxI maxVI lI rI -> let DR min' minV' l' = goDeleteMin maxI maxVI lI rI
                                        in  Bin min' minV' l' r
            Just minV' -> Bin min minV' l r
      where xorCacheMin = xor min k
    
    goDeleteMin max maxV l r = case l of
        Tip -> case r of
            Tip -> DR max maxV r
            Bin min minV l' r' -> DR min minV (Bin max maxV l' r')
        Bin maxI maxVI lI rI -> let DR min minV l' = goDeleteMin maxI maxVI lI rI
                                in  DR min minV (Bin max maxV l' r)
    
    goDeleteMax min minV l r = case r of
        Tip -> case l of
            Tip -> DR min minV l
            Bin max maxV l' r' -> DR max maxV (Bin min minV l' r')
        Bin minI minVI lI rI -> let DR max maxV r' = goDeleteMax minI minVI lI rI
                                in  DR max maxV (Bin min minV l r')

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
{-
-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: WordMap a -> WordMap a -> WordMap a
union = unionWith const

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (a -> a -> a) -> WordMap a -> WordMap a -> WordMap a
unionWith f = unionWithKey (const f)

-- TODO: Come up with some sort of 'mergeWithKey'-like function to avoid the need to duplicate this monstrosity.
-- TODO: Clean up 'unionWithKey' - this is some horribly ugly code.
-- TODO: Explain how 'unionWithKey' works.

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: (Key -> a -> a -> a) -> WordMap a -> WordMap a -> WordMap a
unionWithKey combine m1 m2 = foldrWithKey (insertWithKey combine) m2 m1

-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
intersection :: WordMap a -> WordMap b -> WordMap a
intersection = intersectionWith const

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
intersectionWithKey _ Empty _ = Empty
intersectionWithKey _ _ Empty = Empty
intersectionWithKey combine (NonEmpty min1 root1) (NonEmpty min2 root2) = goL min1 root1 min2 root2
  where
    goL min1 (Tip x1) min2 (Tip x2)
        | min1 == min2 = NonEmpty min1 (Tip (combine min1 x1 x2))
        | otherwise = Empty
    goL min1 (Tip x1) min2 node2
        | min1 < min2 = Empty
        | otherwise = finishLL min1 x1 min2 node2
    goL min1 node1 min2 (Tip x2)
        | min2 < min1 = Empty
        | otherwise = finishLR min2 x2 min1 node1
    goL min1 (Bin max1 l1 r1) min2 (Bin max2 l2 r2)
        | max1 < min2 || max2 < min1 = Empty
        | otherwise = case compareMSB (xor min1 max1) (xor min2 max2) of
            LT | ltMSB (xor min1 max2) (xor min2 max2) -> flipBounds $ goR max1 (Bin min1 l1 r1) max2 r2
               | otherwise -> goL min1 (Bin max1 l1 r1) min2 l2
            EQ -> binL (goL min1 l1 min2 l2) (goR max1 r1 max2 r2)
            GT | ltMSB (xor min2 max1) (xor min1 max1) -> flipBounds $ goR max1 r1 max2 (Bin min2 l2 r2)
               | otherwise -> goL min1 l1 min2 (Bin max2 l2 r2)
    
    goR max1 (Tip x1) max2 (Tip x2)
        | max1 == max2 = NonEmpty max1 (Tip (combine max1 x1 x2))
        | otherwise = Empty
    goR max1 (Tip x1) max2 node2
        | max1 > max2 = Empty
        | otherwise = finishRL max1 x1 max2 node2
    goR max1 node1 max2 (Tip x2)
        | max2 > max1 = Empty
        | otherwise = finishRR max2 x2 max1 node1
    goR max1 (Bin min1 l1 r1) max2 (Bin min2 l2 r2)
        | max1 < min2 || max2 < min1 = Empty
        | otherwise = case compareMSB (xor min1 max1) (xor min2 max2) of
            LT | ltMSB (xor min1 max2) (xor min2 max2) -> goR max1 (Bin min1 l1 r1) max2 r2
               | otherwise -> flipBounds $ goL min1 (Bin max1 l1 r1) min2 l2
            EQ -> binR (goL min1 l1 min2 l2) (goR max1 r1 max2 r2)
            GT | ltMSB (xor min2 max1) (xor min1 max1) -> goR max1 r1 max2 (Bin min2 l2 r2)
               | otherwise -> flipBounds $ goL min1 l1 min2 (Bin max2 l2 r2)
    
    finishLL k v min = endLL k v (xor k min) min
    finishRL k v max = endRL k v (xor k max) max
    finishLR k v min = endLR k v (xor k min) min
    finishRR k v max = endRR k v (xor k max) max
    
    endLL k v !xorCache min (Tip x)
        | k == min = NonEmpty k (Tip (combine k v x))
        | otherwise = Empty
    endLL k v !xorCache min (Bin max l r)
        | k > max = Empty
        | ltMSB xorCache (xor min max) = endLL k v xorCache min l
        | otherwise = finishRL k v max r
    
    endRL k v !xorCache max (Tip x)
        | k == max = NonEmpty k (Tip (combine k v x))
        | otherwise = Empty
    endRL k v !xorCache max (Bin min l r)
        | k < min = Empty
        | ltMSB xorCache (xor min max) = endRL k v xorCache max r
        | otherwise = finishLL k v min l
    
    endLR k v !xorCache min (Tip x)
        | k == min = NonEmpty k (Tip (combine k x v))
        | otherwise = Empty
    endLR k v !xorCache min (Bin max l r)
        | k > max = Empty
        | ltMSB xorCache (xor min max) = endLR k v xorCache min l
        | otherwise = finishRR k v max r
    
    endRR k v !xorCache max (Tip x)
        | k == max = NonEmpty k (Tip (combine k x v))
        | otherwise = Empty
    endRR k v !xorCache max (Bin min l r)
        | k < min = Empty
        | ltMSB xorCache (xor min max) = endRR k v xorCache max r
        | otherwise = finishLR k v min l
-}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (a -> b) -> WordMap a -> WordMap b
map = fmap

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Key -> a -> b) -> WordMap a -> WordMap b
mapWithKey f Empty = Empty
mapWithKey f (NonEmpty min minV root) = NonEmpty min (f min minV) (go root)
  where
    go Tip = Tip
    go (Bin k v l r) = Bin k (f k v) (go l) (go r)


-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative f => (Key -> a -> f b) -> WordMap a -> f (WordMap b)
traverseWithKey f Empty = pure Empty
traverseWithKey f (NonEmpty min minV root) = NonEmpty min <$> f min minV <*> goL min root
  where
    goL min Tip = pure Tip
    goL min (Bin max maxV l r) = (\l' r' maxV' -> Bin max maxV' l' r') <$> goL min l <*> goR max r <*> f max maxV
    
    goR max Tip = pure Tip
    goR max (Bin min minV l r) = Bin min <$> f min minV <*> goL min l <*> goR max r

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
mapAccumWithKey f a Empty = (a, Empty)
mapAccumWithKey f a (NonEmpty min minV root) =
    let (a',  minV') = f a min minV
        (a'', root') = goL min root a'
    in  (a'', NonEmpty min minV' root')
  where
    goL min Tip a = (a, Tip)
    goL min (Bin max maxV l r) a =
        let (a',   l') = goL min l a
            (a'',  r') = goR max r a'
            (a''', maxV') = f a'' max maxV
        in  (a''', Bin max maxV' l' r')
    
    goR max Tip a = (a, Tip)
    goR max (Bin min minV l r) a =
        let (a',   minV') = f a min minV
            (a'',   l') = goL min l a'
            (a''',  r') = goR max r a''
        in  (a''', Bin min minV' l' r')

-- | /O(n)/. The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a, c)) -> a -> WordMap b -> (a, WordMap c)
mapAccumRWithKey f a Empty = (a, Empty)
mapAccumRWithKey f a (NonEmpty min minV root) = 
    let (a',  root') = goL min root a
        (a'', minV') = f a' min minV
    in  (a'', NonEmpty min minV' root')
  where
    goL min Tip a = (a, Tip)
    goL min (Bin max maxV l r) a =
        let (a',   maxV') = f a max maxV
            (a'',  r') = goR max r a'
            (a''', l') = goL min l a''
        in  (a''', Bin max maxV' l' r')
    
    goR max Tip a = (a, Tip)
    goR max (Bin min minV l r) a =
        let (a',   r') = goR max r a
            (a'',  l') = goL min l a'
            (a''', minV') = f a'' min minV
        in  (a''', Bin min minV' l' r')

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
    start Empty = z
    start (NonEmpty _ minV root) = f minV (goL root z)
    
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
    start Empty = z
    start (NonEmpty _ minV root) = goL (f z minV) root
    
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
    start Empty = z
    start (NonEmpty min minV root) = f min minV (goL root z)
    
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
    start Empty = z
    start (NonEmpty min minV root) = goL (f z min minV) root
    
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
    start Empty = mempty
    start (NonEmpty min minV root) = f min minV `mappend` goL root
    
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
    start Empty = z
    start (NonEmpty _ minV root) = f minV $! goL root $! z
    
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
    start Empty = z
    start (NonEmpty _ minV root) = s goL (s f z minV) root
    
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
    start Empty = z
    start (NonEmpty min minV root) = f min minV $! goL root $! z
    
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
    start Empty = z
    start (NonEmpty min minV root) = s goL (s f z min minV) root
    
    goL acc Tip = acc
    goL acc (Bin max maxV l r) = s f (s goR (s goL acc l) r) max maxV
    
    goR acc Tip = acc
    goR acc (Bin min minV l r) = s goR (s goL (s f acc min minV) l) r
    
    s = ($!)

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key, a)] -> WordMap a
fromList = Data.Foldable.foldr (uncurry insert) empty

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: WordMap a -> [(Key, a)]
toList Empty = []
toList (NonEmpty min minV node) = (min, minV) : goL node [] where
    goL Tip rest = rest
    goL (Bin max maxV l r) rest = goL l $ goR r $ (max, maxV) : rest
    
    goR Tip rest = rest
    goR (Bin min minV l r) rest = (min, minV) : (goL l $ goR r $ rest)

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
    start (NonEmpty min minV root)
        | p min minV = NonEmpty min minV (goL root)
        | otherwise = goDeleteL root
    
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
{-
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
-}

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
        Just minV' -> NonEmpty min minV' (goL root)
        Nothing -> goDeleteL root
    
    goL Tip = Tip
    goL (Bin max maxV l r) = case f max maxV of
        Just maxV' -> Bin max maxV' (goL l) (goR r)
        Nothing -> case goDeleteR r of
            Empty -> goL l
            NonEmpty max' maxV' r' -> Bin max' maxV' (goL l) r'
    
    goR Tip = Tip
    goR (Bin min minV l r) = case f min minV of
        Just minV' -> Bin min minV' (goL l) (goR r)
        Nothing -> case goDeleteL l of
            Empty -> goR r
            NonEmpty min' minV' l' -> Bin min' minV' l' (goR r)
    
    goDeleteL Tip = Empty
    goDeleteL (Bin max maxV l r) = case f max maxV of
        Just maxV' -> case goDeleteL l of
            Empty -> case goR r of
                Tip -> NonEmpty max maxV' Tip
                Bin minI minVI lI rI -> NonEmpty minI minVI (Bin max maxV' lI rI)
            NonEmpty min minV l' -> NonEmpty min minV (Bin max maxV' l' (goR r))
        Nothing -> binL (goDeleteL l) (goDeleteR r)
    
    goDeleteR Tip = Empty
    goDeleteR (Bin min minV l r) = case f min minV of
        Just minV' -> case goDeleteR r of
            Empty -> case goL l of
                Tip -> NonEmpty min minV' Tip
                Bin maxI maxVI lI rI -> NonEmpty maxI maxVI (Bin min minV' lI rI)
            NonEmpty max maxV r' -> NonEmpty max maxV (Bin min minV' (goL l) r')
        Nothing -> binR (goDeleteL l) (goDeleteR r)

{-
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
----------------------------

-- | Show the tree that implements the map.
showTree :: Show a => WordMap a -> String
showTree = unlines . aux where
    aux Empty = []
    aux (NonEmpty min minV node) = (show min ++ " " ++ show minV) : auxNode False node
    auxNode _ Tip = ["+-."]
    auxNode lined (Bin bound val l r) = ["+--" ++ show bound ++ " " ++ show val, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent r = prefix : "  " ++ r

valid :: WordMap a -> Bool
valid Empty = True
valid (NonEmpty min _ root) = allKeys (> min) root && goL min root
  where
    goL min Tip = True
    goL min (Bin max _ l r) =
           allKeys (< max) l
        && allKeys (< max) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    goR max Tip = True
    goR max (Bin min _ l r) =
           allKeys (> min) l
        && allKeys (> min) r
        && allKeys (\k -> xor min k < xor k max) l
        && allKeys (\k -> xor min k > xor k max) r
        && goL min l
        && goR max r
    
    allKeys p Tip = True
    allKeys p (Bin b _ l r) = p b && allKeys p l && allKeys p r
{-
-- | /O(1)/. Returns whether the most significant bit of its first
-- argument is less significant than the most significant bit of its
-- second argument.
{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < xor x y

compareMSB :: Word -> Word -> Ordering
compareMSB x y = case compare x y of
    LT | x < xor x y -> LT
    GT | y < xor x y -> GT
    _ -> EQ
-}
{-# INLINE binL #-}
binL :: WordMap a -> WordMap a -> WordMap a
binL Empty r = flipBounds r
binL l Empty = l
binL (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty min minV (Bin max maxV l r)

{-# INLINE binR #-}
binR :: WordMap a -> WordMap a -> WordMap a
binR Empty r = r
binR l Empty = flipBounds l
binR (NonEmpty min minV l) (NonEmpty max maxV r) = NonEmpty max maxV (Bin min minV l r)

{-# INLINE flipBounds #-}
flipBounds Empty = Empty
flipBounds n@(NonEmpty _ _ Tip) = n
flipBounds (NonEmpty b1 v1 (Bin b2 v2 l r)) = NonEmpty b2 v2 (Bin b1 v1 l r)
