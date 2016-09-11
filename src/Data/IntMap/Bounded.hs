{-# LANGUAGE BangPatterns #-}

-- | A reimplementation of Data.IntMap in terms of Data.WordMap.

module Data.IntMap.Bounded (
      module Data.IntMap.Bounded.Lazy
    , insertWith'
    , insertWithKey'
    , fold
    , foldWithKey
) where

import Prelude hiding (foldr)
import Data.IntMap.Bounded.Lazy
import qualified Data.IntMap.Bounded.Strict as S

-- | /Deprecated./ As of version 0.5, replaced by
-- 'Data.IntMap.Strict.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the result of the combining function
-- is evaluated to WHNF before inserted to the map.
{-# INLINE insertWith' #-}
insertWith' :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith' = S.insertWith

-- | /Deprecated./ As of version 0.5, replaced by
-- 'Data.IntMap.Strict.insertWithKey'.
--
-- /O(log n)/. Same as 'insertWithKey', but the result of the combining
-- function is evaluated to WHNF before inserted to the map.
{-# INLINE insertWithKey' #-}
insertWithKey' :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey' = S.insertWithKey

-- | /Deprecated./ As of version 0.5, replaced by 'foldr'.
--
-- /O(n)/. Fold the values in the map using the given
-- right-associative binary operator. This function is an equivalent
-- of 'foldr' and is present for compatibility only.
{-# INLINE fold #-}
fold :: (a -> b -> b) -> b -> IntMap a -> b
fold = foldr

-- | /Deprecated./ As of version 0.5, replaced by 'foldrWithKey'.
--
-- /O(n)/. Fold the keys and values in the map using the given
-- right-associative binary operator. This function is an equivalent
-- of 'foldrWithKey' and is present for compatibility only.
{-# INLINE foldWithKey #-}
foldWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey = foldrWithKey
