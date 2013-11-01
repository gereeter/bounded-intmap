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
    
    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionWithKey
    
    -- * Conversions
    , toList
    , fromList
) where

import Control.DeepSeq
import Data.Foldable hiding (toList)
import Data.Traversable

import qualified Data.WordMap as W

import Prelude hiding (lookup, null)

type Key = Int
newtype IntMap a = IntMap (W.WordMap a)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)

instance Foldable IntMap where
    foldMap f (IntMap m) = foldMap f m

instance Traversable IntMap where
    traverse f (IntMap m) = fmap IntMap (traverse f m)

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

union :: IntMap a -> IntMap a -> IntMap a
union (IntMap m1) (IntMap m2) = IntMap (W.union m1 m2)

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f (IntMap m1) (IntMap m2) = IntMap (W.unionWith f m1 m2)

unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f (IntMap m1) (IntMap m2) = IntMap (W.unionWithKey f' m1 m2) where
    f' k = f (fromIntegral k)

toList :: IntMap a -> [(Int, a)]
toList (IntMap m) = map (\(k, v) -> (fromIntegral k, v)) (W.toList m)

fromList :: [(Int, a)] -> IntMap a
fromList = IntMap . W.fromList . map (\(k, v) -> (fromIntegral k, v))
