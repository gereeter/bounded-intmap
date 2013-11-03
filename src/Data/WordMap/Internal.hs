{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordMap.Internal where

import Control.DeepSeq
import Control.Applicative hiding (empty)

import Data.Monoid
import Data.Foldable hiding (toList)
import Data.Traversable

import Data.Word (Word)
import Data.Bits (xor, (.|.))

import Prelude hiding (foldr, lookup, null, map)

type Key = Word

data WordMap a = NonEmpty {-# UNPACK #-} !Key !(Node a) | Empty deriving (Eq)
data Node a = Bin {-# UNPACK #-} !Key !(Node a) !(Node a) | Tip a deriving (Eq, Show)

instance Show a => Show (WordMap a) where
    show m = "fromList " ++ show (toList m)

instance Functor WordMap where
    fmap f Empty = Empty
    fmap f (NonEmpty min node) = NonEmpty min (fmap f node)

instance Functor Node where
    fmap f (Tip x) = Tip (f x)
    fmap f (Bin bound l r) = Bin bound (fmap f l) (fmap f r)

instance Foldable WordMap where
    foldMap f Empty = mempty
    foldMap f (NonEmpty _ node) = foldMap f node

instance Foldable Node where
    foldMap f (Tip x) = f x
    foldMap f (Bin _ l r) = foldMap f l `mappend` foldMap f r

instance Traversable WordMap where
    traverse f Empty = pure Empty
    traverse f (NonEmpty min node) = NonEmpty min <$> traverse f node

instance Traversable Node where
    traverse f (Tip x) = Tip <$> f x
    traverse f (Bin bound l r) = Bin bound <$> traverse f l <*> traverse f r

instance Monoid (WordMap a) where
    mempty = empty
    mappend = union

instance NFData a => NFData (WordMap a) where
    rnf Empty = ()
    rnf (NonEmpty _ n) = rnf n

instance NFData a => NFData (Node a) where
    rnf (Tip x) = rnf x
    rnf (Bin _ l r) = rnf l `seq` rnf r

-- | /O(1)/. Is the map empty?
null :: WordMap a -> Bool
null Empty = True
null _ = False

-- | /O(n)/. Number of elements in the map.
size :: WordMap a -> Int
size Empty = 0
size (NonEmpty _ node) = sizeNode node where
    sizeNode (Tip _) = 1
    sizeNode (Bin _ l r) = sizeNode l + sizeNode r

-- | /O(1)/. Find the smallest and largest key in the map.
bounds :: WordMap a -> Maybe (Key, Key)
bounds Empty = Nothing
bounds (NonEmpty min (Tip _)) = Just (min, min)
bounds (NonEmpty min (Bin max _ _)) = Just (min, max)

-- TODO: Is there a good way to unify the 'lookup'-like functions?

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> WordMap a -> Bool
member k = k `seq` start
  where
    start Empty = False
    start (NonEmpty min node)
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min (Tip x) = False
    goL !xorCache min (Bin max l r)
        | k > max = False
        | k == max = True
        | xorCache < xor k max = goL xorCache min l
        | otherwise = goR (xor k max) max r
    
    goR !xorCache max (Tip x) = False
    goR !xorCache max (Bin min l r)
        | k < min = False
        | k == min = True
        | xorCache < xor min k = goR xorCache max r
        | otherwise = goL (xor min k) min l

-- | /O(min(n,W))/. Is the key not a member of the map?
notMember :: Key -> WordMap a -> Bool
notMember k = k `seq` start
  where
    start Empty = True
    start (NonEmpty min node)
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min (Tip x) = True
    goL !xorCache min (Bin max l r)
        | k > max = True
        | k == max = False
        | xorCache < xor k max = goL xorCache min l
        | otherwise = goR (xor k max) max r
    
    goR !xorCache max (Tip x) = True
    goR !xorCache max (Bin min l r)
        | k < min = True
        | k == min = False
        | xorCache < xor min k = goR xorCache max r
        | otherwise = goL (xor min k) min l

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Key -> WordMap a -> Maybe a
lookup k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | k < min = Nothing
        | otherwise = goL (xor min k) min node
    
    goL !xorCache min (Tip x)
        | k == min = Just x
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | k > max = Nothing
        | xorCache < xor k max = goL xorCache min l
        | otherwise = goR (xor k max) max r
    
    goR !xorCache max (Tip x)
        | k == max = Just x
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | k < min = Nothing
        | xorCache < xor min k = goR xorCache max r
        | otherwise = goL (xor min k) min l

-- | /O(min(n,W))/. The expression @findWithDefault def k map@ returns
-- the value at key @k@ or returns @def@ when the key is not an element
-- of the map. 
findWithDefault :: a -> Key -> WordMap a -> a
findWithDefault def k = k `seq` start
  where
    start Empty = def
    start (NonEmpty min node)
        | k < min = def
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | k == min = x
        | otherwise = def
    goL !xorCache min (Bin max l r)
        | k > max = def
        | ltMSB xorCache (xor min max) = goL xorCache min l
        | otherwise = startR max r
    
    goR !xorCache max (Tip x)
        | k == max = x
        | otherwise = def
    goR !xorCache max (Bin min l r)
        | k < min = def
        | ltMSB xorCache (xor min max) = goR xorCache max r
        | otherwise = startL min l

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
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | min < k = Just (min, x)
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | max < k = Just (max, getMaxV r)
        | ltMSB xorCache (xor min max) = goL xorCache min l
        | otherwise = startR max r min l
    
    goR !xorCache max (Tip x) fMin fallback
        | max < k = Just (max, x)
        | otherwise = Just (getMax fMin fallback)
    goR !xorCache max (Bin min l r) fMin fallback
        | min >= k = Just (getMax fMin fallback)
        | ltMSB xorCache (xor min max) = goR xorCache max r min l
        | otherwise = startL min l
    
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
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | min <= k = Just (min, x)
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | max <= k = Just (max, getMaxV r)
        | ltMSB xorCache (xor min max) = goL xorCache min l
        | otherwise = startR max r min l
    
    goR !xorCache max (Tip x) fMin fallback
        | max <= k = Just (max, x)
        | otherwise = Just (getMax fMin fallback)
    goR !xorCache max (Bin min l r) fMin fallback
        | min > k = Just (getMax fMin fallback)
        | ltMSB xorCache (xor min max) = goR xorCache max r min l
        | otherwise = startL min l
    
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
        | otherwise = startR max (Bin min l r)
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x) fMax fallback
        | min > k = Just (min, x)
        | otherwise = Just (getMin fMax fallback)
    goL !xorCache min (Bin max l r) fMax fallback
        | max <= k = Just (getMin fMax fallback)
        | ltMSB xorCache (xor min max) = goL xorCache min l max r
        | otherwise = startR max r
    
    goR !xorCache max (Tip x)
        | max > k = Just (max, x)
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | min > k = Just (min, getMinV l)
        | ltMSB xorCache (xor min max) = goR xorCache max r
        | otherwise = startL min l max r
    
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
        | otherwise = startR max (Bin min l r)
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x) fMax fallback
        | min >= k = Just (min, x)
        | otherwise = Just (getMin fMax fallback)
    goL !xorCache min (Bin max l r) fMax fallback
        | max < k = Just (getMin fMax fallback)
        | ltMSB xorCache (xor min max) = goL xorCache min l max r
        | otherwise = startR max r
    
    goR !xorCache max (Tip x)
        | max >= k = Just (max, x)
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | min >= k = Just (min, getMinV l)
        | ltMSB xorCache (xor min max) = goR xorCache max r
        | otherwise = startL min l max r
    
    getMin max (Tip x) = (max, x)
    getMin max (Bin min l r) = (min, getMinV l)
    
    getMinV (Tip x) = x
    getMinV (Bin b l r) = getMinV l
    
-- | /O(1)/. The empty map.
empty :: WordMap a
empty = Empty

-- | /O(1)/. A map of one element.
singleton :: Key -> a -> WordMap a
singleton k v = NonEmpty k (Tip v)

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value
-- is replaced with the supplied value. 
insert :: Key -> a -> WordMap a -> WordMap a
insert = insertWith const

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
insertWith combine !k v Empty = NonEmpty k (Tip v)
insertWith combine !k v (NonEmpty min node)
    | k < min = NonEmpty k (endL (xor min k) min node)
    | otherwise = NonEmpty min (goL (xor min k) min node)
  where
    
    goL !xorCache min (Tip x)
        | k == min = Tip (combine v x)
        | otherwise = Bin k (Tip x) (Tip v)
    goL !xorCache min (Bin max l r)
        | k > max = if xor min max < xorCacheMax then Bin k (Bin max l r) (Tip v) else Bin k l (endR xorCacheMax max r)
        | xorCache < xorCacheMax = Bin max (goL xorCache min l) r
        | otherwise = Bin max l (goR xorCacheMax max r)
      where
        xorCacheMax = xor k max

    goR !xorCache max (Tip x)
        | k == max = Tip (combine v x)
        | otherwise = Bin k (Tip v) (Tip x)
    goR !xorCache max (Bin min l r)
        | k < min = if xor min max < xorCacheMin then Bin k (Tip v) (Bin min l r) else Bin k (endL xorCacheMin min l) r
        | xorCache < xorCacheMin = Bin min l (goR xorCache max r)
        | otherwise = Bin min (goL xorCacheMin min l) r
      where
        xorCacheMin = xor min k
    
    endL !xorCache min (Tip x) = Bin min (Tip v) (Tip x)
    endL !xorCache min (Bin max l r)
        | xor min max < xorCache = Bin max (Tip v) (Bin min l r)
        | otherwise = Bin max (endL xorCache min l) r

    endR !xorCache max (Tip x) = Bin max (Tip x) (Tip v)
    endR !xorCache max (Bin min l r)
        | xor min max < xorCache = Bin min (Bin max l r) (Tip v)
        | otherwise = Bin min l (endR xorCache max r)

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
    start m@(NonEmpty min (Tip x))
        | k == min = Empty
        | otherwise = m
    start m@(NonEmpty min (Bin max l r))
        | k < min = m
        | k == min = let DR min' root' = goDeleteMin max l r in NonEmpty min' root'
        | otherwise = NonEmpty min (goL (xor min k) min (Bin max l r))
    
    goL !xorCache min n@(Tip _) = n
    goL !xorCache min n@(Bin max l r)
        | k > max = Bin max l r
        | k == max = case r of
            Tip _ -> l
            Bin minI lI rI -> let DR max' r' = goDeleteMax minI lI rI
                              in  Bin max' l r'
        | xorCache < xorCacheMax = Bin max (goL xorCache min l) r
        | otherwise = Bin max l (goR xorCacheMax max r)
      where xorCacheMax = xor k max
    
    goR !xorCache max n@(Tip _) = n
    goR !xorCache max n@(Bin min l r)
        | k < min = n
        | k == min = case l of
            Tip _ -> r
            Bin maxI lI rI -> let DR min' l' = goDeleteMin maxI lI rI
                              in  Bin min' l' r
        | xorCache < xorCacheMin = Bin min l (goR xorCache max r)
        | otherwise = Bin max (goL xorCacheMin min l) r
      where xorCacheMin = xor min k
    
    goDeleteMin max l r = case l of
        Tip _ -> case r of
            Tip _ -> DR max r
            Bin min l' r' -> DR min (Bin max l' r')
        Bin maxI lI rI -> let DR min l' = goDeleteMin maxI lI rI
                          in  DR min (Bin max l' r)
    
    goDeleteMax min l r = case r of
        Tip _ -> case l of
            Tip _ -> DR min l
            Bin max l' r' -> DR max (Bin min l' r')
        Bin minI lI rI -> let DR max r' = goDeleteMax minI lI rI
                          in  DR max (Bin min l r')

data DeleteResult a = DR {-# UNPACK #-} !Key !(Node a)

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
    start m@(NonEmpty min node)
        | k < min = m
        | otherwise = NonEmpty min (startL min node)
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min n@(Tip x)
        | k == min = Tip (f x)
        | otherwise = n
    goL !xorCache min n@(Bin max l r)
        | k > max = n
        | ltMSB xorCache (xor min max) = Bin max (goL xorCache min l) r
        | otherwise = Bin max l (startR max r)
    
    goR !xorCache max n@(Tip x)
        | k == max = Tip (f x)
        | otherwise = n
    goR !xorCache max n@(Bin min l r)
        | k < min = n
        | ltMSB xorCache (xor min max) = Bin min l (goR xorCache max r)
        | otherwise = Bin min (startL min l) r

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
    start m@(NonEmpty min node)
        | k < min = m
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | k == min = case f x of
            Nothing -> Empty
            Just x' -> NonEmpty min (Tip x')
        | otherwise = NonEmpty min (Tip x)
    goL !xorCache min n@(Bin max l r)
        | k > max = NonEmpty min n
        | ltMSB xorCache (xor min max) = binLL max (goL xorCache min l) r
        | otherwise = binRL min l (startR max r)
    
    goR !xorCache max (Tip x)
        | k == max = case f x of
            Nothing -> Empty
            Just x' -> NonEmpty max (Tip x')
        | otherwise = NonEmpty max (Tip x)
    goR !xorCache max n@(Bin min l r)
        | k < min = NonEmpty max n
        | ltMSB xorCache (xor min max) = binRR min l (goR xorCache max r)
        | otherwise = binLR max (startL min l) r
    
    binLL max Empty (Tip x) = NonEmpty max (Tip x)
    binLL max Empty (Bin min l r) = NonEmpty min (Bin max l r)
    binLL max (NonEmpty min l) r = NonEmpty min (Bin max l r)
    
    binLR max Empty (Tip x) = NonEmpty max (Tip x)
    binLR max Empty (Bin min l r) = NonEmpty max (Bin min l r)
    binLR max (NonEmpty min l) r = NonEmpty max (Bin min l r)
    
    binRL min (Tip x) Empty = NonEmpty min (Tip x)
    binRL min (Bin max l r) Empty = NonEmpty min (Bin max l r)
    binRL min l (NonEmpty max r) = NonEmpty min (Bin max l r)
    
    binRR min (Tip x) Empty = NonEmpty min (Tip x)
    binRR min (Bin max l r) Empty = NonEmpty max (Bin min l r)
    binRR min l (NonEmpty max r) = NonEmpty max (Bin min l r)

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
unionWithKey _ Empty r = r
unionWithKey _ l Empty = l
unionWithKey combine (NonEmpty min1 node1) (NonEmpty min2 node2) = NonEmpty (Prelude.min min1 min2) (goL min1 node1 min2 node2)
  where
    goL min1 (Tip x1) min2 (Tip x2) = case compare min1 min2 of
        LT -> Bin min2 (Tip x1) (Tip x2)
        EQ -> Tip x1
        GT -> Bin min1 (Tip x2) (Tip x1)
    goL min1 (Tip x1) min2 (Bin max2 l2 r2) = insertLL min1 x1 min2 (Bin max2 l2 r2)
    goL min1 (Bin max1 l1 r1) min2 (Tip x2) = insertLR min2 x2 min1 (Bin max1 l1 r1)
    goL min1 (Bin max1 l1 r1) min2 (Bin max2 l2 r2)
        | max1 < min2 && ltMSB (xor min1 max1 .|. xor min2 max2) (xor max1 min2) = Bin max2 (Bin max1 l1 r1) (Bin min2 l2 r2)
        | max2 < min1 && ltMSB (xor min1 max1 .|. xor min2 max2) (xor max2 min1) = Bin max1 (Bin max2 l2 r2) (Bin min1 l1 r1)
        | otherwise = case compareMSB (xor min1 max1) (xor min2 max2) of
            LT | ltMSB (xor min1 max2) (xor min2 max2) -> Bin (Prelude.max max1 max2) l2 (goR max1 (Bin min1 l1 r1) max2 r2)
               | otherwise -> Bin (Prelude.max max1 max2) (goL min1 (Bin max1 l1 r1) min2 l2) r2
            EQ -> Bin (Prelude.max max1 max2) (goL min1 l1 min2 l2) (goR max1 r1 max2 r2)
            GT | ltMSB (xor min2 max1) (xor min1 max1) -> Bin (Prelude.max max1 max2) l1 (goR max1 r1 max2 (Bin min2 l2 r2))
               | otherwise -> Bin (Prelude.max max1 max2) (goL min1 l1 min2 (Bin max1 l2 r2)) r1
    
    goR max1 (Tip x1) max2 (Tip x2) = case compare max1 max2 of
        LT -> Bin max1 (Tip x1) (Tip x2)
        EQ -> Tip x1
        GT -> Bin max2 (Tip x2) (Tip x1)
    goR max1 (Tip x1) max2 (Bin min2 l2 r2) = insertRL max1 x1 max2 (Bin min2 l2 r2)
    goR max1 (Bin min1 l1 r1) max2 (Tip x2) = insertRR max2 x2 max1 (Bin min1 l1 r1)
    goR max1 (Bin min1 l1 r1) max2 (Bin min2 l2 r2)
        | max1 < min2 && ltMSB (xor min1 max1 .|. xor min2 max2) (xor max1 min2) = Bin min1 (Bin max1 l1 r1) (Bin min2 l2 r2)
        | max2 < min1 && ltMSB (xor min1 max1 .|. xor min2 max2) (xor max2 min1) = Bin min2 (Bin max2 l2 r2) (Bin min1 l1 r1)
        | otherwise = case compareMSB (xor min1 max1) (xor min2 max2) of
            LT | ltMSB (xor min1 max2) (xor min2 max2) -> Bin (Prelude.min min1 min2) l2 (goR max1 (Bin min1 l1 r1) max2 r2)
               | otherwise -> Bin (Prelude.min min1 min2) (goL min1 (Bin max1 l1 r1) min2 l2) r2
            EQ -> Bin (Prelude.min min1 min2) (goL min1 l1 min2 l2) (goR max1 r1 max2 r2)
            GT | ltMSB (xor min2 max1) (xor min1 max1) -> Bin (Prelude.min min1 min2) l1 (goR max1 r1 max2 (Bin min2 l2 r2))
               | otherwise -> Bin (Prelude.min min1 min2) (goL min1 l1 min2 (Bin max1 l2 r2)) r1
    
    insertLL k v min = goInsertLL k v (xor k min) min
    insertRL k v max = goInsertRL k v (xor k max) max
    insertLR k v min = goInsertLR k v (xor k min) min
    insertRR k v max = goInsertRR k v (xor k max) max
    
    goInsertLL k v !xorCache min (Tip x)
        | k == min = Tip (combine k v x)
        | otherwise = Bin k (Tip x) (Tip v)
    goInsertLL k v !xorCache min (Bin max l r)
        | k > max = if ltMSB (xor min max) xorCache then Bin k (Bin max l r) (Tip v) else Bin k l (finishR k v max r)
        | ltMSB xorCache (xor min max) = Bin max (goInsertLL k v xorCache min l) r
        | otherwise = Bin max l (insertRL k v max r)

    goInsertRL k v !xorCache max (Tip x)
        | k == max = Tip (combine k v x)
        | otherwise = Bin k (Tip v) (Tip x)
    goInsertRL k v !xorCache max (Bin min l r)
        | k < min = if ltMSB (xor min max) xorCache then Bin k (Tip v) (Bin min l r) else Bin k (finishL k v min l) r
        | ltMSB xorCache (xor min max) = Bin min l (goInsertRL k v xorCache max r)
        | otherwise = Bin min (insertLL k v min l) r
    
    goInsertLR k v !xorCache min (Tip x)
        | k == min = Tip (combine k x v)
        | otherwise = Bin k (Tip x) (Tip v)
    goInsertLR k v !xorCache min (Bin max l r)
        | k > max = if ltMSB (xor min max) xorCache then Bin k (Bin max l r) (Tip v) else Bin k l (finishR k v max r)
        | ltMSB xorCache (xor min max) = Bin max (goInsertLR k v xorCache min l) r
        | otherwise = Bin max l (insertRR k v max r)

    goInsertRR k v !xorCache max (Tip x)
        | k == max = Tip (combine k x v)
        | otherwise = Bin k (Tip v) (Tip x)
    goInsertRR k v !xorCache max (Bin min l r)
        | k < min = if ltMSB (xor min max) xorCache then Bin k (Tip v) (Bin min l r) else Bin k (finishL k v min l) r
        | ltMSB xorCache (xor min max) = Bin min l (goInsertRR k v xorCache max r)
        | otherwise = Bin min (insertLR k v min l) r
    
    finishL k v min = endL k v (xor k min) min
    finishR k v max = endR k v (xor k max) max
    
    endL k v !xorCache min (Tip x) = Bin min (Tip v) (Tip x)
    endL k v !xorCache min (Bin max l r)
        | ltMSB (xor min max) xorCache = Bin max (Tip v) (Bin min l r)
        | otherwise = Bin max (endL k v xorCache min l) r

    endR k v !xorCache max (Tip x) = Bin max (Tip x) (Tip v)
    endR k v !xorCache max (Bin min l r)
        | ltMSB (xor min max) xorCache = Bin min (Bin max l r) (Tip v)
        | otherwise = Bin min l (endR k v xorCache max r)

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
    
    binL Empty Empty = Empty
    binL Empty r = flipBounds r
    binL l Empty = l
    binL (NonEmpty min l) (NonEmpty max r) = NonEmpty min (Bin max l r)
    
    binR Empty Empty = Empty
    binR Empty r = r
    binR l Empty = flipBounds l
    binR (NonEmpty min l) (NonEmpty max r) = NonEmpty max (Bin min l r)
    
    flipBounds Empty = Empty
    flipBounds n@(NonEmpty b1 (Tip x)) = n
    flipBounds (NonEmpty b1 (Bin b2 l r)) = NonEmpty b2 (Bin b1 l r)
    
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
mapWithKey f (NonEmpty min root) = NonEmpty min (goL min root)
  where
    goL min (Tip x) = Tip (f min x)
    goL min (Bin max l r) = Bin max (goL min l) (goR max r)
    
    goR max (Tip x) = Tip (f max x)
    goR max (Bin min l r) = Bin min (goL min l) (goR max r)


-- | /O(n)/.
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative f => (Key -> a -> f b) -> WordMap a -> f (WordMap b)
traverseWithKey f Empty = pure Empty
traverseWithKey f (NonEmpty min root) = NonEmpty min <$> goL min root
  where
    goL min (Tip x) = Tip <$> f min x
    goL min (Bin max l r) = Bin max <$> goL min l <*> goR max r
    
    goR max (Tip x) = Tip <$> f max x
    goR max (Bin min l r) = Bin max <$> goL min l <*> goR max r

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
mapAccumWithKey f a (NonEmpty min root) = let (a', root') = goL min root a in (a', NonEmpty min root')
  where
    goL min (Tip x) a =
        let (a', x') = f a min x
        in  (a', Tip x')
    goL min (Bin max l r) a =
        let (a',  l') = goL min l a
            (a'', r') = goR max r a'
        in  (a'', Bin max l' r')
    
    goR max (Tip x) a =
        let (a', x') = f a max x
        in  (a', Tip x')
    goR max (Bin min l r) a =
        let (a',  l') = goL min l a
            (a'', r') = goR max r a'
        in  (a'', Bin min l' r')

-- | /O(n)/. The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a, c)) -> a -> WordMap b -> (a, WordMap c)
mapAccumRWithKey f a Empty = (a, Empty)
mapAccumRWithKey f a (NonEmpty min root) = let (a', root') = goL min root a in (a', NonEmpty min root')
  where
    goL min (Tip x) a =
        let (a', x') = f a min x
        in  (a', Tip x')
    goL min (Bin max l r) a =
        let (a',  r') = goR max r a
            (a'', l') = goL min l a'
        in  (a'', Bin max l' r')
    
    goR max (Tip x) a =
        let (a', x') = f a max x
        in  (a', Tip x')
    goR max (Bin min l r) a =
        let (a',  r') = goR max r a
            (a'', l') = goL min l a'
        in  (a'', Bin min l' r')

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key, a)] -> WordMap a
fromList = foldr (uncurry insert) empty

-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: WordMap a -> [(Key, a)]
toList Empty = []
toList (NonEmpty min node) = goL min node [] where
    goL min (Tip x) rest = (min, x) : rest
    goL min (Bin max l r) rest = goL min l (goR max r rest)
    
    goR max (Tip x) rest = (max, x) : rest
    goR max (Bin min l r) rest = goL min l (goR max r rest)

----------------------------

-- | Show the tree that implements the map.
showTree :: Show a => WordMap a -> String
showTree = unlines . aux where
    aux Empty = []
    aux (NonEmpty min node) = show min : auxNode False node
    auxNode _ (Tip x) = ["+->" ++ show x]
    auxNode lined (Bin bound l r) = ["+--" ++ show bound, prefix : "  |"] ++ fmap indent (auxNode True l) ++ [prefix : "  |"] ++ fmap indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent r = prefix : "  " ++ r

valid :: WordMap a -> Bool
valid Empty = True
valid (NonEmpty min root) = allKeys (> min) root && goL min root
  where
    goL min (Tip x) = True
    goL min (Bin max l r) =
           allKeys (< max) r
        && allKeys (\k -> not $ ltMSB (xor k max) (xor min max)) l
        && allKeys (\k ->       ltMSB (xor k max) (xor min max)) r
        && goL min l
        && goR max r
    
    goR max (Tip x) = True
    goR max (Bin min l r) =
           allKeys (> min) l
        && allKeys (\k -> not $ ltMSB (xor k max) (xor min max)) l
        && allKeys (\k ->       ltMSB (xor k max) (xor min max)) r
        && goL min l
        && goR max r
    
    allKeys p (Tip x) = True
    allKeys p (Bin b l r) = p b && allKeys p l && allKeys p r

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
