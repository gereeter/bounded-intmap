{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordMap (
    -- * Map type
      WordMap, Key
    
    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    
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
    
    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey
    
    -- * Conversions
    , toList
    , fromList
    
    -- * Debugging
    , showTree
    , valid
) where

import Data.WordMap.Internal

import Prelude hiding (lookup, null)
