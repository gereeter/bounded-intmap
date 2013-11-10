{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordSet.Internal where

import Control.DeepSeq

import qualified Data.List as List

import Data.Word (Word)
import Data.Bits
import Data.Bits.Extras (trailingZeros, leadingZeros)

import Prelude hiding (foldr, foldl, null, map, filter)

type Key = Word
type BitMap = Word
type Prefix = Word

data WordSet = NonEmpty {-# UNPACK #-} !Key !Node | Empty deriving (Eq)
data Node = Bin {-# UNPACK #-} !Key !Node !Node | BM {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap | Tip deriving (Eq, Show)

instance Show WordSet where
    show m = "fromList " ++ show (toList m)

instance NFData WordSet

-- | /O(n+m)/. See 'difference'.
(\\) :: WordSet -> WordSet -> WordSet
(\\) = difference

-- | /O(1)/. Is the map empty?
null :: WordSet -> Bool
null Empty = True
null _ = False

-- | /O(n)/. Number of elements in the map.
size :: WordSet -> Int
size Empty = 0
size (NonEmpty _ node) = sizeNode node where
    sizeNode Tip = 1
    sizeNode (BM _ bm) = fromIntegral $ popCount bm
    sizeNode (Bin _ l r) = sizeNode l + sizeNode r

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> WordSet -> Bool
member k = k `seq` start
  where
    start Empty = False
    start (NonEmpty min node)
        | k < min = False
        | k == min = True
        | otherwise = goL (xor min k) node
    
    goL !xorCache Tip = False
    goL !xorCache (BM pre bm) = prefixOf k == pre && (bm .&. (1 `unsafeShiftL` suffixOf k) /= 0)
    goL !xorCache (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | otherwise = k == max
      where xorCacheMax = xor k max
    
    goR !xorCache Tip = False
    goR !xorCache (BM pre bm) = prefixOf k == pre && (bm .&. (1 `unsafeShiftL` suffixOf k) /= 0)
    goR !xorCache (Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | otherwise = k == min
      where xorCacheMin = xor min k

-- | /O(min(n,W))/. Is the key not a member of the map?
notMember :: Key -> WordSet -> Bool
notMember k = k `seq` start
  where
    start Empty = True
    start (NonEmpty min node)
        | k < min = True
        | k == min = False
        | otherwise = goL (xor min k) node
    
    goL !xorCache Tip = True
    goL !xorCache (BM pre bm) = prefixOf k /= pre || (bm .&. (1 `unsafeShiftL` suffixOf k) == 0)
    goL !xorCache (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | otherwise = k == max
      where xorCacheMax = xor k max
    
    goR !xorCache Tip = True
    goR !xorCache (BM pre bm) = prefixOf k /= pre && (bm .&. (1 `unsafeShiftL` suffixOf k) == 0)
    goR !xorCache (Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then goR xorCache r
                    else goL xorCacheMin l
        | otherwise = k == min
      where xorCacheMin = xor min k
{-
-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3
lookupLT :: Key -> WordSet -> Maybe Key
lookupLT k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | min >= k = Nothing
        | otherwise = Just (goL (xor min k) min node)
    
    goL !xorCache min Tip = min
    goL !xorCache min (Bin max l r)
        | max < k = max
        | xorCache < xorCacheMax = goL xorCache min l
        | otherwise = goR xorCacheMax max r min l
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip fMin fallback = getMax fMin fallback
    goR !xorCache max (Bin min l r) fMin fallback
        | min >= k = getMax fMin fallback
        | xorCache < xorCacheMin = goR xorCache max r min l
        | otherwise = goL xorCacheMin min l
      where xorCacheMin = xor min k
    
    getMax min Tip = min
    getMax min (Bin max _ _) = max

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5
lookupLE :: Key -> WordSet -> Maybe Key
lookupLE k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min node)
        | min > k = Nothing
        | otherwise = Just (goL (xor min k) min node)
    
    goL !xorCache min Tip = min
    goL !xorCache min (Bin max l r)
        | max <= k = max
        | xorCache < xorCacheMax = goL xorCache min l
        | otherwise = goR xorCacheMax max r min l
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip fMin fallback = getMax fMin fallback
    goR !xorCache max (Bin min l r) fMin fallback
        | min > k = getMax fMin fallback
        | xorCache < xorCacheMin = goR xorCache max r min l
        | otherwise = goL xorCacheMin min l
      where xorCacheMin = xor min k
    
    getMax min Tip = min
    getMax min (Bin max _ _) = max

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing
lookupGT :: Key -> WordSet -> Maybe Key
lookupGT k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min Tip)
        | min > k = Just min
        | otherwise = Nothing
    start (NonEmpty min (Bin max l r))
        | min > k = Just min
        | max > k = Just (goR (xor k max) max (Bin min l r))
        | otherwise = Nothing
    
    goL !xorCache min Tip fMax fallback = getMin fMax fallback
    goL !xorCache min (Bin max l r) fMax fallback
        | max <= k = getMin fMax fallback
        | xorCache < xorCacheMax = goL xorCache min l fMax fallback
        | otherwise = goR xorCacheMax max r
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = max
    goR !xorCache max (Bin min l r)
        | min > k = min
        | xorCache < xorCacheMin = goR xorCache max r
        | otherwise = goL xorCacheMin min l max r
      where xorCacheMin = xor min k
    
    getMin max Tip = max
    getMin max (Bin min _ _) = min

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing
lookupGE :: Key -> WordSet -> Maybe Key
lookupGE k = k `seq` start
  where
    start Empty = Nothing
    start (NonEmpty min Tip)
        | min >= k = Just min
        | otherwise = Nothing
    start (NonEmpty min (Bin max l r))
        | min >= k = Just min
        | max >= k = Just (goR (xor k max) max (Bin min l r))
        | otherwise = Nothing
    
    goL !xorCache min Tip fMax fallback = getMin fMax fallback
    goL !xorCache min (Bin max l r) fMax fallback
        | max < k = getMin fMax fallback
        | xorCache < xorCacheMax = goL xorCache min l fMax fallback
        | otherwise = goR xorCacheMax max r
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = max
    goR !xorCache max (Bin min l r)
        | min >= k = min
        | xorCache < xorCacheMin = goR xorCache max r
        | otherwise = goL xorCacheMin min l max r
      where xorCacheMin = xor min k
    
    getMin max Tip = max
    getMin max (Bin min _ _) = min
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
    
    goL !xorCache min Tip = singleNode k
    goL !xorCache min n@(BM pre bm)
        | prefixOf k == pre = BM pre (setBit bm (suffixOf k))
        | k > pre = Bin k n Tip
        | otherwise = let DR max bm' = deleteMaxBM pre bm
                      in Bin max (singleNode k) (bitmap pre bm')
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k (Bin max l r) Tip
                    else Bin k l (endR xorCacheMax max r)
        | otherwise = n
      where xorCacheMax = xor k max

    goR !xorCache max Tip = singleNode k
    goR !xorCache max n@(BM pre bm)
        | prefixOf k == pre = BM pre (setBit bm (suffixOf k))
        | k < pre = Bin k Tip n
        | otherwise = let DR min bm' = deleteMinBM pre bm
                      in Bin min (bitmap pre bm') (singleNode k)
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
        finishL Tip = singleNode min
        finishL n@(BM pre bm)
            | prefixOf min == pre = BM pre (setBit bm (suffixOf min))
            | otherwise = let DR max bm' = deleteMaxBM pre bm
                          in Bin max (singleNode min) (bitmap pre bm')
        finishL (Bin max l r)
            | xor min max < xorCache = Bin max Tip (Bin min l r)
            | otherwise = Bin max (finishL l) r

    endR !xorCache max = finishR
      where
        finishR Tip = singleNode max
        finishR n@(BM pre bm)
            | prefixOf max == pre = BM pre (setBit bm (suffixOf max))
            | otherwise = let DR min bm' = deleteMinBM pre bm
                          in Bin min (bitmap pre bm') (singleNode max)
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
    start m@(NonEmpty min (BM pre bm))
        | k == min = let DR min' bm' = deleteMinBM pre bm
                     in NonEmpty min' (bitmap pre bm')
        | prefixOf k == pre = NonEmpty min (bitmap pre (clearBit bm (suffixOf k)))
        | otherwise = m
    start m@(NonEmpty min root@(Bin max l r))
        | k < min = m
        | k == min = let DR min' root' = deleteMinNode max l r in NonEmpty min' root'
        | otherwise = NonEmpty min (goL (xor min k) min root)
    
    goL !xorCache min Tip = Tip
    goL !xorCache min n@(BM pre bm)
        | prefixOf k == pre = bitmap pre (clearBit bm (suffixOf k))
        | otherwise = n
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case r of
            Tip -> l
            BM pre bm -> let DR max' bm' = deleteMaxBM pre bm
                         in if bm' == 0
                            then case l of
                                Tip -> singleNode max'
                                _ -> Bin max' l Tip
                            else Bin max' l (BM pre bm')
            Bin minI lI rI -> let DR max' r' = deleteMaxNode minI lI rI
                              in  Bin max' l r'
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip = Tip
    goR !xorCache max n@(BM pre bm)
        | prefixOf k == pre = bitmap pre (clearBit bm (suffixOf k))
        | otherwise = n
    goR !xorCache max n@(Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min l (goR xorCache max r)
                    else Bin min (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case l of
            Tip -> r
            BM pre bm -> let DR min' bm' = deleteMinBM pre bm
                         in if bm' == 0
                            then case r of
                                Tip -> singleNode min'
                                _ -> Bin min' Tip r
                            else Bin min' (BM pre bm') r
            Bin maxI lI rI -> let DR min' l' = deleteMinNode maxI lI rI
                              in  Bin min' l' r
      where xorCacheMin = xor min k

-- TODO: Does a strict pair work? My guess is not, as GHC was already
-- unboxing the tuple, but it would be simpler to use one of those.
-- | Without this specialized type (I was just using a tuple), GHC's
-- CPR correctly unboxed the tuple, but it couldn't unbox the returned
-- Key, leading to lots of inefficiency (3x slower than stock Data.WordMap)
data DeleteResult a = DR {-# UNPACK #-} !Key !a

-- TODO: Optimize
-- | /O(n+m)/. The union of two sets.
union :: WordSet -> WordSet -> WordSet
union l r = foldr' insert r l

-- | The union of a list of sets.
unions :: [WordSet] -> WordSet
unions = List.foldl' union empty

-- | /O(n+m)/. Difference between two sets.
difference :: WordSet -> WordSet -> WordSet
difference = foldr' delete

-- | /O(n+m)/. The intersection of two sets.
intersection :: WordSet -> WordSet -> WordSet
intersection l = filter (`member` l)

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
    start (NonEmpty min root) = f min $ goL root z
    
    goL Tip acc = acc
    goL (BM pre bm) acc = goBM pre bm acc
    goL (Bin max l r) acc = goL l $ goR r $ f max acc
    
    goR Tip acc = acc
    goR (BM pre bm) acc = goBM pre bm acc
    goR (Bin min l r) acc = f min $ goL l $ goR r acc
    
    goBM pre bm acc = let DR min bm' = deleteMinBM pre bm
                  in if bm' == 0
                     then f min $ acc
                     else f min $ goBM pre bm' acc

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
    start (NonEmpty min root) = goL root $ f z min
    
    goL Tip acc = acc
    goL (BM pre bm) acc = goBM pre bm acc
    goL (Bin max l r) acc = (f $ goR r $ goL l acc) $ max
    
    goR Tip acc = acc
    goR (BM pre bm) acc = goBM pre bm acc
    goR (Bin min l r) acc = goR r $ goL l $ f acc min
    
    goBM pre bm acc = let DR min bm' = deleteMinBM pre bm
                  in if bm' == 0
                     then f acc min
                     else goBM pre bm' $ f acc min

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> WordSet -> b
foldr' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = f min $! goL root z
    
    goL Tip acc = acc
    goL (BM pre bm) acc = goBM pre bm acc
    goL (Bin max l r) acc = goL l $! goR r $! f max acc
    
    goR Tip acc = acc
    goR (BM pre bm) acc = goBM pre bm acc
    goR (Bin min l r) acc = f min $! goL l $! goR r acc
    
    goBM pre bm acc = let DR min bm' = deleteMinBM pre bm
                  in if bm' == 0
                     then f min $! acc
                     else f min $! goBM pre bm' acc

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> WordSet -> a
foldl' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = goL root $! f z min
    
    goL Tip acc = acc
    goL (BM pre bm) acc = goBM pre bm acc
    goL (Bin max l r) acc = (f $! goR r $! goL l acc) $! max
    
    goR Tip acc = acc
    goR (BM pre bm) acc = goBM pre bm acc
    goR (Bin min l r) acc = goR r $! goL l $! f acc min
    
    goBM pre bm acc = let DR min bm' = deleteMinBM pre bm
                  in if bm' == 0
                     then f acc min
                     else goBM pre bm' $! f acc min

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

-- | /O(n)/. Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Key] -> WordSet
fromAscList = fromList

-- | /O(n)/. Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Key] -> WordSet
fromDistinctAscList = fromList

-- TODO: Optimize
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Key -> Bool) -> WordSet -> WordSet
filter p = fromDistinctAscList . List.filter p . toAscList

-- TODO: Optimize
-- | /O(n)/. partition the set according to some predicate.
partition :: (Key -> Bool) -> WordSet -> (WordSet, WordSet)
partition p s = let (t, f) = List.partition p (toAscList s)
                in  (fromDistinctAscList t, fromDistinctAscList f)

-- | /O(1)/. The minimal element of the set.
findMin :: WordSet -> Word
findMin Empty = error "findMin: empty set has no minimal element"
findMin (NonEmpty min _) = min

-- | /O(1)/. The maximal element of a set.
findMax :: WordSet -> Word
findMax Empty = error "findMax: empty set has no maximal element"
findMax (NonEmpty min Tip) = min
findMax (NonEmpty _ (Bin max _ _)) = max

deleteMin :: WordSet -> WordSet
deleteMin Empty = Empty
deleteMin (NonEmpty _ Tip) = Empty
deleteMin (NonEmpty _ (BM pre bm)) = let DR min bm' = deleteMinBM pre bm
                                     in if bm' == 0
                                        then NonEmpty min Tip
                                        else NonEmpty min (BM pre bm')
deleteMin (NonEmpty _ (Bin max l r)) = let DR min root = deleteMinNode max l r
                                       in  NonEmpty min root

deleteMax :: WordSet -> WordSet
deleteMax Empty = Empty
deleteMax (NonEmpty _ Tip) = Empty
deleteMax (NonEmpty min (BM pre bm)) = let DR _ bm' = deleteMaxBM pre bm
                                       in if bm' == 0
                                          then NonEmpty min Tip
                                          else NonEmpty min (BM pre bm')
deleteMax (NonEmpty min (Bin _ l r)) = let DR _ root = deleteMaxNode min l r
                                       in NonEmpty min root

----------------------------

-- | Show the tree that implements the map.
showTree :: WordSet -> String
showTree = unlines . aux where
    aux Empty = []
    aux (NonEmpty min node) = show min : auxNode False node
    auxNode _ Tip = ["+-."]
    auxNode _ (BM pre bm) = ["+-" ++ show pre ++ " " ++ show bm]
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

--------------------------

{-# INLINE deleteMinNode #-}
deleteMinNode :: Key -> Node -> Node -> DeleteResult Node
deleteMinNode max l r = case l of
    Tip -> case r of
       Tip -> DR max r
       BM pre bm -> let DR min bm' = deleteMinBM pre bm
                    in DR min (bitmap pre bm')
       Bin min l' r' -> DR min (Bin max l' r')
    BM pre bm -> let DR min bm' = deleteMinBM pre bm
                 in if bm' == 0
                    then case r of
                        Tip -> DR min (singleNode max)
                        _ -> DR min (Bin max Tip r)
                    else DR min (Bin max (BM pre bm') r)
    Bin maxI lI rI -> let DR min l' = deleteMinNode maxI lI rI
                      in  DR min (Bin max l' r)

{-# INLINE deleteMaxNode #-}
deleteMaxNode :: Key -> Node -> Node -> DeleteResult Node
deleteMaxNode min l r = case r of
    Tip -> case l of
        Tip -> DR min l
        BM pre bm -> let DR max bm' = deleteMaxBM pre bm
                     in DR max (bitmap pre bm')
        Bin max l' r' -> DR max (Bin min l' r')
    BM pre bm -> let DR max bm' = deleteMaxBM pre bm
                 in if bm' == 0
                    then case l of
                        Tip -> DR max (singleNode min)
                        _ -> DR max (Bin min l Tip)
                    else DR max (Bin min l (BM pre bm'))
    Bin minI lI rI -> let DR max r' = deleteMaxNode minI lI rI
                      in  DR max (Bin min l r')

{-# INLINE deleteMinBM #-}
deleteMinBM :: Prefix -> BitMap -> DeleteResult BitMap
deleteMinBM pre bm = let suf = trailingZeros bm
                         bm' = clearBit bm (fromIntegral suf)
                         min = pre .|. fromIntegral suf
                     in DR min bm'

{-# INLINE deleteMaxBM #-}
deleteMaxBM :: Prefix -> BitMap -> DeleteResult BitMap
deleteMaxBM pre bm = let suf = bitSize bm - 1 - fromIntegral (leadingZeros bm)
                         bm' = clearBit bm suf
                         max = pre .|. fromIntegral suf
                     in DR max bm'

singleNode :: Key -> Node
singleNode k = BM (prefixOf k) (setBit 0 (suffixOf k))

bitmap :: Prefix -> BitMap -> Node
bitmap _ 0 = Tip
bitmap pre bm = BM pre bm

prefixMask, suffixMask :: Word
prefixMask = complement suffixMask
suffixMask = fromIntegral (bitSize (undefined :: Word)) - 1

prefixOf :: Word -> Word
prefixOf = (prefixMask .&.)

suffixOf :: Word -> Int
suffixOf = fromIntegral . (suffixMask .&.)
