{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordSet (
    -- * Map type
      WordSet, Key
    
    -- * Operators
    , (\\)
    
    -- * Query
    , null
    , size
    , member
    , notMember
{-    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE-}
    
    -- * Construction
    , empty
    , singleton
    , insert
    , delete
    
    -- * Combine
    , union
    , unions
    , difference
    , intersection
    
    -- * Filter
    , filter
    , partition
    
    -- * Map
    , map
    
    -- * Folds
    , foldr
    , foldl
    
    -- ** Strict folds
    , foldr'
    , foldl'
    
    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    
    -- * Conversion
    -- ** List
    , elems
    , toList
    , fromList
    -- ** Ordered list
    , toAscList
    , toDescList
    , fromAscList
    , fromDistinctAscList
    
    -- * Debugging
    {-, showTree
    , valid-}
) where

import Data.WordSet.Internal

import Prelude hiding (foldr, foldl, null, map, filter)
