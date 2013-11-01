{-# LANGUAGE BangPatterns #-}

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordMap where

import Control.DeepSeq
import Control.Applicative hiding (empty)

import Data.Monoid
import Data.Foldable hiding (toList)
import Data.Traversable

import Data.Word (Word)
import Data.Bits (xor)

import Prelude hiding (foldr, lookup)

type Key = Word

data WordMap a = Empty | NonEmpty {-# UNPACK #-} !Key !(Node a) deriving (Eq)
data Node a = Tip a | Bin {-# UNPACK #-} !Key !(Node a) !(Node a) deriving (Eq, Show)

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

instance NFData a => NFData (WordMap a) where
    rnf Empty = ()
    rnf (NonEmpty _ n) = rnf n

instance NFData a => NFData (Node a) where
    rnf (Tip x) = rnf x
    rnf (Bin _ l r) = rnf l `seq` rnf r

-- | /O(1)/. The empty map.
empty :: WordMap a
empty = Empty

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

-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Key -> WordMap a -> Maybe a
lookup k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | k < min = Nothing
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | k == min = Just x
        | otherwise = Nothing
    goL !xorCache min (Bin max l r)
        | k > max = Nothing
        | ltMSB xorCache (xor min max) = startR max r
        | otherwise = goL xorCache min l
    
    goR !xorCache max (Tip x)
        | k == max = Just x
        | otherwise = Nothing
    goR !xorCache max (Bin min l r)
        | k < min = Nothing
        | ltMSB xorCache (xor min max) = goR xorCache max r
        | otherwise = startL min l

-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value
-- is replaced with the supplied value. 
insert :: Key -> a -> WordMap a -> WordMap a
insert !k v Empty = NonEmpty k (Tip v)
insert !k v (NonEmpty min node)
    | k < min = NonEmpty k (finishL min node)
    | otherwise = NonEmpty min (startL min node)
  where
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | k == min = Tip v
        | otherwise = Bin k (Tip x) (Tip v)
    goL !xorCache min (Bin max l r)
        | k > max = if ltMSB (xor min max) xorCache then Bin k (Bin max l r) (Tip v) else Bin k l (finishR max r)
        | ltMSB xorCache (xor min max) = Bin max l (startR max r)
        | otherwise = Bin max (goL xorCache min l) r

    goR !xorCache max (Tip x)
        | k == max = Tip v
        | otherwise = Bin k (Tip v) (Tip x)
    goR !xorCache max (Bin min l r)
        | k < min = if ltMSB (xor min max) xorCache then Bin k (Tip v) (Bin min l r) else Bin k (finishL min l) r
        | ltMSB xorCache (xor min max) = Bin min l (goR xorCache max r)
        | otherwise = Bin min (startL min l) r
    
    finishL min = endL (xor k min) min
    finishR max = endR (xor k max) max
    
    endL !xorCache min (Tip x) = Bin min (Tip v) (Tip x)
    endL !xorCache min (Bin max l r)
        | ltMSB (xor min max) xorCache = Bin max (Tip v) (Bin min l r)
        | otherwise = Bin max (endL xorCache min l) r

    endR !xorCache max (Tip x) = Bin max (Tip x) (Tip v)
    endR !xorCache max (Bin min l r)
        | ltMSB (xor min max) xorCache = Bin min (Bin max l r) (Tip v)
        | otherwise = Bin min l (endR xorCache max r)

-- | /O(min(n,W))/. Delete a key and its value from the map.
-- When the key is not a member of the map, the original map is returned.
delete :: Key -> WordMap a -> WordMap a
delete k = k `seq` start
  where
    start Empty = Empty
    start m@(NonEmpty min node)
        | k < min = m
        | otherwise = startL min node
    
    startL min = goL (xor k min) min
    startR max = goR (xor k max) max
    
    goL !xorCache min (Tip x)
        | k == min = Empty
        | otherwise = NonEmpty min (Tip x)
    goL !xorCache min n@(Bin max l r)
        | k > max = NonEmpty min n
        | ltMSB xorCache (xor min max) = binRL min l (startR max r)
        | otherwise = binLL max (goL xorCache min l) r
    
    goR !xorCache max (Tip x)
        | k == max = Empty
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
    auxNode lined (Bin bound l r) = ["+--" ++ show bound, prefix : "  |"] ++ map indent (auxNode True l) ++ [prefix : "  |"] ++ map indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent r = prefix : "  " ++ r

-- | /O(1)/. Returns whether the most significant bit of its first
-- argument is less significant than the most significant bit of its
-- second argument.
{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < xor x y
