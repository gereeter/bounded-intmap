{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.WordMap that seems to be 1.4-4x faster.

module Data.WordSet.Internal where

import Control.DeepSeq

import qualified Data.List as List

import Data.Word (Word)
import Data.Bits
import Data.Bits.Extras (trailingZeros, leadingZeros)

import Prelude hiding (foldr, foldl, null, map, filter, min, max)

type Key = Word
type BitMap = Word

data WordSet = NonEmpty {-# UNPACK #-} !Key !Node | Empty deriving (Eq)
data Node = Bin {-# UNPACK #-} !Key !Node !Node | BM {-# UNPACK #-} !Key {-# UNPACK #-} !BitMap | Tip deriving (Eq, Show)

instance Show WordSet where
    show m = "fromList " ++ show (toList m)

-- instance NFData WordSet

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
    
    goL !_ Tip = False
    goL !xorCache (BM _ bm) = xorCache <= suffixMask && (bm .&. getSuf k /= 0)
    goL !xorCache (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | otherwise = k == max
      where xorCacheMax = xor k max
    
    goR !_ Tip = False
    goR !xorCache (BM _ bm) = xorCache <= suffixMask && (bm .&. getSuf k /= 0)
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
    
    goL !_ Tip = True
    goL !xorCache (BM _ bm) = xorCache <= suffixMask && (bm .&. getSuf k == 0)
    goL !xorCache (Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then goL xorCache l
                    else goR xorCacheMax r
        | otherwise = k == max
      where xorCacheMax = xor k max
    
    goR !_ Tip = True
    goR !xorCache (BM _ bm) = xorCache <= suffixMask && (bm .&. getSuf k == 0)
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
    
    goL !xorCache min Tip
        | xorCache <= suffixMask = BM k (getSuf min .|. getSuf k)
        | otherwise = Bin k Tip Tip
    goL !xorCache _ n@(BM max bm)
        | k <= max = BM max (bm .|. getSuf k)
        | xorCache <= suffixMask = BM k (bm .|. getSuf k) 
        | otherwise = Bin k n Tip
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = if xor min max < xorCacheMax
                    then Bin k (Bin max l r) Tip
                    else Bin k l (endR xorCacheMax max r)
        | otherwise = n
      where xorCacheMax = xor k max
    
    goR !xorCache max Tip
        | xorCache <= suffixMask = BM k (getSuf k .|. getSuf max)
        | otherwise = Bin k Tip Tip
    goR !xorCache _ n@(BM min bm)
        | k >= min = BM min (bm .|. getSuf k)
        | xorCache <= suffixMask = BM k (bm .|. getSuf k)
        | otherwise = Bin k Tip n
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
        finishL Tip
            | xorCache <= suffixMask = BM min (getSuf k .|. getSuf min)
            | otherwise = Bin min Tip Tip
        finishL (BM max bm)
            | xorCache <= suffixMask = BM max (bm .|. getSuf k)
            | otherwise = Bin max Tip (BM min bm)
        finishL (Bin max l r)
            | xor min max < xorCache = Bin max Tip (Bin min l r)
            | otherwise = Bin max (finishL l) r

    endR !xorCache max = finishR
      where
        finishR Tip
            | xorCache <= suffixMask = BM max (getSuf max .|. getSuf k)
            | otherwise = Bin max Tip Tip
        finishR (BM min bm)
            | xorCache <= suffixMask = BM min (bm .|. getSuf k)
            | otherwise = Bin min (BM max bm) Tip
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
    start m@(NonEmpty min (BM max bm))
        | k < min || k > max = m
        | k == min = if bm' == getSuf max
                     then NonEmpty max Tip
                     else NonEmpty (prefixOf k .|. getMinBM bm') (BM max bm')
        | k == max = if bm' == getSuf min
                     then NonEmpty min Tip
                     else NonEmpty min (BM (prefixOf k .|. getMaxBM bm') bm')
        | otherwise = NonEmpty min (BM max bm')
      where
        bm' = bm .&. complement (getSuf k)
    start m@(NonEmpty min root@(Bin max l r))
        | k < min = m
        | k == min = let DR min' root' = goDeleteMin min max l r in NonEmpty min' root'
        | otherwise = NonEmpty min (goL (xor min k) min root)
    
    goL !_        _   Tip = Tip
    goL !xorCache min n@(BM max bm)
        | k < max = BM max bm'
        | k == max = if bm' == getSuf min
                     then Tip
                     else BM (prefixOf max .|. getMaxBM bm') bm'
        | otherwise = n
      where
        bm' = bm .&. complement (getSuf k)
    goL !xorCache min n@(Bin max l r)
        | k < max = if xorCache < xorCacheMax
                    then Bin max (goL xorCache min l) r
                    else Bin max l (goR xorCacheMax max r)
        | k > max = n
        | otherwise = case r of
            Tip -> l
            BM minI bm | bm' == getSuf minI -> Bin minI l Tip
                       | otherwise -> Bin (prefixOf max .|. getMaxBM bm') l (BM minI bm')
              where
                bm' = bm .&. complement (getSuf max)
            Bin minI lI rI -> let DR max' r' = goDeleteMax max minI lI rI
                              in  Bin max' l r'
      where xorCacheMax = xor k max
    
    goR !_        _   Tip = Tip
    goR !xorCache max n@(BM min bm)
        | k > min = BM min bm'
        | k == min = if bm' == getSuf max
                     then Tip
                     else BM (prefixOf min .|. getMinBM bm') bm'
        | otherwise = n
      where
        bm' = bm .&. complement (getSuf k)
    goR !xorCache max n@(Bin min l r)
        | k > min = if xorCache < xorCacheMin
                    then Bin min l (goR xorCache max r)
                    else Bin min (goL xorCacheMin min l) r
        | k < min = n
        | otherwise = case l of
            Tip -> r
            BM maxI bm | bm' == getSuf maxI -> Bin maxI Tip r
                       | otherwise -> Bin (prefixOf min .|. getMinBM bm') (BM maxI bm') r
              where
                bm' = bm .&. complement (getSuf min)
            Bin maxI lI rI -> let DR min' l' = goDeleteMin min maxI lI rI
                              in  Bin min' l' r
      where xorCacheMin = xor min k
    
    goDeleteMin min max l r = case l of
        Tip -> case r of
            Tip -> DR max Tip
            BM minI bm -> DR minI (BM max bm)
            Bin minI lI rI -> DR minI (Bin max lI rI)
        BM maxI bm | bm' == getSuf maxI -> DR maxI (Bin max Tip r)
                   | otherwise -> DR (prefixOf min .|. getMinBM bm') (Bin max (BM maxI bm') r)
          where
            bm' = bm .&. complement (getSuf min)
        Bin maxI lI rI -> let DR min' l' = goDeleteMin min maxI lI rI
                          in  DR min' (Bin max l' r)
    
    goDeleteMax max min l r = case r of
        Tip -> case l of
            Tip -> DR min Tip
            BM maxI bm -> DR maxI (BM min bm)
            Bin maxI lI rI -> DR maxI (Bin min lI rI)
        BM minI bm | bm' == getSuf minI -> DR minI (Bin min l Tip)
                   | otherwise -> DR (prefixOf max .|. getMaxBM bm') (Bin min l (BM minI bm'))
          where
            bm' = bm .&. complement (getSuf max)
        Bin minI lI rI -> let DR max' r' = goDeleteMax max minI lI rI
                          in  DR max' (Bin min l r')

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
    start (NonEmpty min root) = f min $ goL min root z
    
    goL _ Tip acc = acc
    goL min (BM _ bm) acc = goBM (prefixOf min) (bm .&. complement (getSuf min)) acc
    goL min (Bin max l r) acc = goL min l $ goR max r $ f max acc
    
    goR _ Tip acc = acc
    goR max (BM _ bm) acc = goBM (prefixOf max) (bm .&. complement (getSuf max)) acc
    goR max (Bin min l r) acc = f min $ goL min l $ goR max r acc
    
    goBM pre = pre `seq` loop
      where
        loop 0 acc = acc
        loop bm acc = let min = pre .|. getMinBM bm
                          bm' = bm .&. complement (getSuf min)
                      in f min (loop bm' acc)

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
    start (NonEmpty min root) = goL min (f z min) root
    
    goL _ acc Tip = acc
    goL min acc (BM _ bm) = goBM (prefixOf min) acc (bm .&. complement (getSuf min))
    goL min acc (Bin max l r) = f (goR max (goL min acc l) r) max
    
    goR _ acc Tip = acc
    goR max acc (BM _ bm) = goBM (prefixOf max) acc (bm .&. complement (getSuf max))
    goR max acc (Bin min l r) = goR max (goL min (f acc min) l) r
    
    goBM pre = pre `seq` loop
      where
        loop acc 0 = acc
        loop acc bm = let min = pre .|. getMinBM bm
                          bm' = bm .&. complement (getSuf min)
                      in loop (f acc min) bm'

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> WordSet -> b
foldr' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = f min $! goL min root z
    
    goL _ Tip acc = acc
    goL min (BM _ bm) acc = goBM (prefixOf min) (bm .&. complement (getSuf min)) $! acc
    goL min (Bin max l r) acc = goL min l $! goR max r $! f max $! acc
    
    goR _ Tip acc = acc
    goR max (BM _ bm) acc = goBM (prefixOf max) (bm .&. complement (getSuf max)) $! acc
    goR max (Bin min l r) acc = f min $! goL min l $! goR max r acc
    
    goBM pre = pre `seq` loop
      where
        loop 0 acc = acc
        loop bm acc = let min = pre .|. getMinBM bm
                          bm' = bm .&. complement (getSuf min)
                      in f min $! loop bm' $! acc

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> WordSet -> a
foldl' f z = start
  where
    start Empty = z
    start (NonEmpty min root) = s (goL min) (f z min) root
    
    goL _ acc Tip = acc
    goL min acc (BM _ bm) = s (goBM (prefixOf min)) acc (bm .&. complement (getSuf min))
    goL min acc (Bin max l r) = s f (s (goR max) (s (goL min) acc l) r) max
    
    goR _ acc Tip = acc
    goR max acc (BM _ bm) = goBM (prefixOf max) acc (bm .&. complement (getSuf max))
    goR max acc (Bin min l r) = s (goR max) (s (goL min) (s f acc min) l) r
    
    goBM pre = pre `seq` loop
      where
        loop acc 0 = acc
        loop acc bm = let min = pre .|. getMinBM bm
                          bm' = bm .&. complement (getSuf min)
                      in s loop (s f acc min) bm'
    
    s = ($!)

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
findMax (NonEmpty _ (BM max _)) = max
findMax (NonEmpty _ (Bin max _ _)) = max

deleteMin :: WordSet -> WordSet
deleteMin Empty = Empty
deleteMin m = delete (findMin m) m

deleteMax :: WordSet -> WordSet
deleteMax Empty = Empty
deleteMax m = delete (findMin m) m

----------------------------
{-
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
-}
--------------------------

getMinBM, getMaxBM :: BitMap -> Word
getMinBM bm = fromIntegral (trailingZeros bm)
getMaxBM bm = fromIntegral (bitSize bm) - 1 - fromIntegral (leadingZeros bm)

prefixMask, suffixMask :: Word
prefixMask = complement suffixMask
suffixMask = fromIntegral (bitSize (undefined :: Word)) - 1

prefixOf, suffixOf :: Word -> Word
prefixOf = (.&. prefixMask)
suffixOf = (.&. suffixMask)

getSuf :: Word -> Word
getSuf k = 1 `unsafeShiftL` fromIntegral (suffixOf k)
