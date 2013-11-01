{-# LANGUAGE BangPatterns #-}

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordMap where

import Control.DeepSeq

import Data.Word (Word)
import Data.Bits (xor)

import Prelude hiding (lookup)

data WordMap a = Empty | NonEmpty {-# UNPACK #-} !Word !(Node a) deriving (Show)
data Node a = Tip a | Bin {-# UNPACK #-} !Word !(Node a) !(Node a) deriving (Show)

instance Functor WordMap where
    fmap f Empty = Empty
    fmap f (NonEmpty min node) = NonEmpty min (fmap f node)

instance Functor Node where
    fmap f (Tip x) = Tip (f x)
    fmap f (Bin bound l r) = Bin bound (fmap f l) (fmap f r)

instance NFData a => NFData (WordMap a) where
    rnf Empty = ()
    rnf (NonEmpty _ n) = rnf n

instance NFData a => NFData (Node a) where
    rnf (Tip x) = rnf x
    rnf (Bin _ l r) = rnf l `seq` rnf r

empty :: WordMap a
empty = Empty

null :: WordMap a -> Bool
null Empty = True
null _ = False

size :: WordMap a -> Int
size Empty = 0
size (NonEmpty _ node) = sizeNode node where
    sizeNode (Tip _) = 1
    sizeNode (Bin _ l r) = sizeNode l + sizeNode r

bounds :: WordMap a -> Maybe (Word, Word)
bounds Empty = Nothing
bounds (NonEmpty min (Tip _)) = Just (min, min)
bounds (NonEmpty min (Bin max _ _)) = Just (min, max)

{-# INLINE lookupInt #-}
lookupInt :: Int -> WordMap a -> Maybe a
lookupInt = lookup . fromIntegral

lookup :: Word -> WordMap a -> Maybe a
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

{-# INLINE insertInt #-}
insertInt :: Int -> a -> WordMap a -> WordMap a
insertInt = insert . fromIntegral

insert :: Word -> a -> WordMap a -> WordMap a
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

{-# INLINE deleteInt #-}
deleteInt :: Int -> WordMap a -> WordMap a
deleteInt = delete . fromIntegral

delete :: Word -> WordMap a -> WordMap a
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

fromListInt :: [(Int, a)] -> WordMap a
fromListInt = foldr (uncurry insertInt) empty

fromList :: [(Word, a)] -> WordMap a
fromList = foldr (uncurry insert) empty

toList :: WordMap a -> [(Word, a)]
toList Empty = []
toList (NonEmpty min node) = goL min node [] where
    goL min (Tip x) rest = (min, x) : rest
    goL min (Bin max l r) rest = goL min l (goR max r rest)
    
    goR max (Tip x) rest = (max, x) : rest
    goR max (Bin min l r) rest = goL min l (goR max r rest)

----------------------------

showTree :: Show a => WordMap a -> String
showTree = unlines . aux where
    aux Empty = []
    aux (NonEmpty min node) = show min : auxNode False node
    auxNode _ (Tip x) = ["+->" ++ show x]
    auxNode lined (Bin bound l r) = ["+--" ++ show bound, prefix : "  |"] ++ map indent (auxNode True l) ++ [prefix : "  |"] ++ map indent (auxNode False r)
      where
        prefix = if lined then '|' else ' '
        indent r = prefix : "  " ++ r

{-# INLINE ltMSB #-}
ltMSB :: Word -> Word -> Bool
ltMSB x y = x < y && x < xor x y
