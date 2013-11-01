{-# LANGUAGE BangPatterns #-}

-- | A reimplementation of Data.IntMap in terms of Data.WordMap.

module Data.IntMap.Bounded where

import Control.DeepSeq

import qualified Data.WordMap as W

type Key = Int
newtype IntMap a = IntMap (W.WordMap a)

instance Functor IntMap where
    fmap f (IntMap m) = IntMap (fmap f m)

instance NFData a => NFData (IntMap a) where
    rnf (IntMap m) = rnf m

lookup :: Key -> IntMap a -> Maybe a
lookup k (IntMap m) = W.lookup (fromIntegral k) m

insert :: Key -> a -> IntMap a -> IntMap a
insert k v (IntMap m) = IntMap (W.insert (fromIntegral k) v m)

delete :: Key -> IntMap a -> IntMap a
delete k (IntMap m) = IntMap (W.delete (fromIntegral k) m)

fromList :: [(Int, a)] -> IntMap a
fromList = IntMap . W.fromList . map (\(k, v) -> (fromIntegral k, v))

toList :: IntMap a -> [(Int, a)]
toList (IntMap m) = map (\(k, v) -> (fromIntegral k, v)) (W.toList m)
