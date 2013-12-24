{-# LANGUAGE BangPatterns #-}

-- TODO: Add some comments describing how this implementation works.

-- | A reimplementation of Data.IntMap that seems to be 1.4-4x faster.

module Data.WordMap (
    -- * Map type
      WordMap, Key
    
    -- * Operators
    , (!)
    
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
    , insertLookupWithKey
    
    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    
    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions
    , unionsWith
    
    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey
    
    -- ** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey
    
    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic
    
    -- ** Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey
    
    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    
    -- * Conversion
    -- ** Lists
    , toList
    , fromList
    
    -- * Filter
    , filter
    , filterWithKey
{-    , partition
    , partitionWithKey-}
    , mapMaybe
    , mapMaybeWithKey
    , splitLookup
    
    -- * Debugging
    , showTree
    , valid
) where

import Data.WordMap.Internal

import Prelude hiding (foldr, foldl, lookup, null, map, filter)
