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
import Data.Foldable hiding (toList)
import Data.Traversable
import Data.Monoid

import Data.Bits (xor)

import Data.WordMap.Internal (WordMap(..), Node(..))
import qualified Data.WordMap as W

import Prelude hiding (lookup, null, map, filter)

type Key = Int
newtype IntMap a = IntMap (W.WordMap a)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)
{-
instance Foldable IntMap where
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
member k (IntMap m) = W.member (fromIntegral k) m

notMember :: Key -> IntMap a -> Bool
notMember k (IntMap m) = W.notMember (fromIntegral k) m

lookup :: Key -> IntMap a -> Maybe a
lookup k (IntMap m) = W.lookup (fromIntegral k) m

findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k (IntMap m) = W.findWithDefault def (fromIntegral k) m

empty :: IntMap a
empty = IntMap W.empty

singleton :: Key -> a -> IntMap a
singleton k v = IntMap (W.singleton (fromIntegral k) v)

insert :: Key -> a -> IntMap a -> IntMap a
insert k v (IntMap m) = IntMap (W.insert (fromIntegral k) v m)

insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f k v (IntMap m) = IntMap (W.insertWith f (fromIntegral k) v m)

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k = insertWith (f k) k

delete :: Key -> IntMap a -> IntMap a
delete k (IntMap m) = IntMap (W.delete (fromIntegral k) m)

adjust :: (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k (IntMap m) = IntMap (W.adjust f (fromIntegral k) m)

adjustWithKey :: (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f k (IntMap m) = IntMap (W.adjustWithKey f' (fromIntegral k) m) where
    f' = f . fromIntegral

update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f k (IntMap m) = IntMap (W.update f (fromIntegral k) m)

updateWithKey :: (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k (IntMap m) = IntMap (W.updateWithKey f' (fromIntegral k) m) where
    f' = f . fromIntegral

union :: IntMap a -> IntMap a -> IntMap a
union (IntMap m1) (IntMap m2) = IntMap (W.union m1 m2)

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f (IntMap m1) (IntMap m2) = IntMap (W.unionWith f m1 m2)

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.unionWithKey f' m1 m2) where
    f' k = f (fromIntegral k)

intersection :: IntMap a -> IntMap b -> IntMap a
intersection (IntMap m1) (IntMap m2) = IntMap (W.intersection m1 m2)

intersectionWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWith f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWith f m1 m2)

intersectionWithKey :: (Key -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.intersectionWithKey f' m1 m2) where
    f' k = f (fromIntegral k)

map :: (a -> b) -> IntMap a -> IntMap b
map = fmap

mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f (IntMap m) = IntMap (W.mapWithKey f' m) where
    f' = f . fromIntegral

fromList :: [(Int, a)] -> IntMap a
fromList = IntMap . W.fromList . fmap (\(k, v) -> (fromIntegral k, v))

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p (IntMap m) = IntMap (W.filter p m)

filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey p (IntMap m) = IntMap (W.filterWithKey (p . fromIntegral) m)

partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition p (IntMap m) = let (t, f) = W.partition p m in (IntMap t, IntMap f)

partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey p (IntMap m) = let (l, r) = W.partitionWithKey (p . fromIntegral) m in (IntMap l, IntMap r)

mapMaybe :: (a -> Maybe b) -> IntMap a -> IntMap b
mapMaybe f (IntMap m) = IntMap (W.mapMaybe f m)

mapMaybeWithKey :: (Key -> a -> Maybe b) -> IntMap a -> IntMap b
mapMaybeWithKey f (IntMap m) = IntMap (W.mapMaybeWithKey (f . fromIntegral) m)

mapEither :: (a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEither f (IntMap m) = let (l, r) = W.mapEither f m in (IntMap l, IntMap r)

mapEitherWithKey :: (Key -> a -> Either b c) -> IntMap a -> (IntMap b, IntMap c)
mapEitherWithKey f (IntMap m) = let (l, r) = W.mapEitherWithKey (f . fromIntegral) m in (IntMap l, IntMap r)

showTree :: Show a => IntMap a -> String
showTree (IntMap m) = W.showTree m

valid :: IntMap a -> Bool
valid (IntMap m) = W.valid m
