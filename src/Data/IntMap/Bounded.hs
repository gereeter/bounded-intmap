{-# LANGUAGE BangPatterns #-}

-- | A reimplementation of Data.IntMap in terms of Data.WordMap.

module Data.IntMap.Bounded (
    -- * Map type
      IntMap, Key
    
    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    
    -- * Construction
    , empty
    , singleton
    
    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    
    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    
    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionWithKey
    
    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey
    
    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    
    -- ** Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    
    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    
    -- * Conversion
{-    , toList-}
    , fromList
    
    -- * Filter
    , filter
    , filterWithKey
    , partition
    , partitionWithKey
    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey
    
    -- * Debugging
    , showTree
    , valid
) where

import Control.DeepSeq
import Control.Applicative hiding (empty)
import qualified Data.Foldable as Foldable (Foldable(..))
import Data.Traversable
import Data.Monoid

import Data.Bits (xor)

import Data.WordMap.Internal (WordMap(..), Node(..))
import qualified Data.WordMap as W

import Data.Word (Word)

import Prelude hiding (lookup, null, map, filter, foldl, foldr, min, max)

type Key = Int
newtype IntMap a = IntMap (W.WordMap a)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)

instance Foldable.Foldable IntMap where
    foldr = foldr
    foldl = foldl
    foldr' = foldr'
    foldl' = foldl'
{-
    foldMap f (IntMap Empty) = mempty
    foldMap f (IntMap (NonEmpty min (Tip x))) = f x
    foldMap f (IntMap (NonEmpty min (Bin max l r)))
        | fromIntegral (xor min max) < (0 :: Int) = foldMap f r `mappend` foldMap f l
        | otherwise = foldMap f l `mappend` foldMap f r
    

instance Traversable IntMap where
    traverse f (IntMap Empty) = pure (IntMap Empty)
    traverse f (IntMap (NonEmpty min (Tip x))) = IntMap . NonEmpty min . Tip <$> f x
    traverse f (IntMap (NonEmpty min (Bin max l r)))
        | fromIntegral (xor min max) < (0 :: Int) = IntMap . NonEmpty min <$> (flip (Bin max) <$> traverse f r <*> traverse f l)
        | otherwise = IntMap . NonEmpty min <$> (Bin max <$> traverse f l <*> traverse f r)
-}
instance Monoid (IntMap a) where
    mempty = IntMap mempty
    mappend (IntMap m1) (IntMap m2) = IntMap (mappend m1 m2)

instance NFData a => NFData (IntMap a) where
    rnf (IntMap m) = rnf m

null :: IntMap a -> Bool
null (IntMap m) = W.null m

size :: IntMap a -> Int
size (IntMap m) = W.size m

member :: Key -> IntMap a -> Bool
member k (IntMap m) = W.member (i2w k) m

notMember :: Key -> IntMap a -> Bool
notMember k (IntMap m) = W.notMember (i2w k) m

lookup :: Key -> IntMap a -> Maybe a
lookup k (IntMap m) = W.lookup (i2w k) m

findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k (IntMap m) = W.findWithDefault def (i2w k) m

empty :: IntMap a
empty = IntMap W.empty

singleton :: Key -> a -> IntMap a
singleton k v = IntMap (W.singleton (i2w k) v)

insert :: Key -> a -> IntMap a -> IntMap a
insert k v (IntMap m) = IntMap (W.insert (i2w k) v m)

insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f k v (IntMap m) = IntMap (W.insertWith f (i2w k) v m)

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k = insertWith (f k) k

delete :: Key -> IntMap a -> IntMap a
delete k (IntMap m) = IntMap (W.delete (i2w k) m)

adjust :: (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k (IntMap m) = IntMap (W.adjust f (i2w k) m)

adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f k (IntMap m) = IntMap (W.adjustWithKey f' (i2w k) m) where
    f' = f . w2i

update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f k (IntMap m) = IntMap (W.update f (i2w k) m)

updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k (IntMap m) = IntMap (W.updateWithKey f' (i2w k) m) where
    f' = f . w2i

union :: IntMap a -> IntMap a -> IntMap a
union (IntMap m1) (IntMap m2) = IntMap (W.union m1 m2)

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f (IntMap m1) (IntMap m2) = IntMap (W.unionWith f m1 m2)

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.unionWithKey f' m1 m2) where
    f' = f . w2i

intersection :: IntMap a -> IntMap b -> IntMap a
intersection (IntMap m1) (IntMap m2) = IntMap (W.intersection m1 m2)

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWith f m1 m2)

intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWithKey f' m1 m2) where
    f' = f . w2i

map :: (a -> b) -> IntMap a -> IntMap b
map = fmap

mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f (IntMap m) = IntMap (W.mapWithKey f' m) where
    f' = f . w2i

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

fromList :: [(Int, a)] -> IntMap a
fromList = IntMap . W.fromList . fmap (\(k, v) -> (i2w k, v))

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p (IntMap m) = IntMap (W.filter p m)

filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p (IntMap m) = IntMap (W.filterWithKey (p . w2i) m)

partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p (IntMap m) = let (t, f) = W.partition p m in (IntMap t, IntMap f)

partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p (IntMap m) = let (l, r) = W.partitionWithKey (p . w2i) m in (IntMap l, IntMap r)

mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f (IntMap m) = IntMap (W.mapMaybe f m)

mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (IntMap m) = IntMap (W.mapMaybeWithKey (f . w2i) m)

mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f (IntMap m) = let (l, r) = W.mapEither f m in (IntMap l, IntMap r)

mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f (IntMap m) = let (l, r) = W.mapEitherWithKey (f . w2i) m in (IntMap l, IntMap r)

showTree :: Show a => IntMap a -> String
showTree (IntMap m) = W.showTree m

valid :: IntMap a -> Bool
valid (IntMap m) = W.valid m

----------------------

i2w :: Int -> Word
i2w = fromIntegral

w2i :: Word -> Int
w2i = fromIntegral
