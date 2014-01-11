{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntMap as M
import qualified Data.IntMap.Bounded as W
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do
    let denseM = M.fromAscList elems :: M.IntMap Int
        denseW = W.fromList elems :: W.IntMap Int
        sparseM = M.fromAscList sElems :: M.IntMap Int
        sparseW = W.fromList sElems :: W.IntMap Int
        sparseM' = M.fromAscList sElemsSearch :: M.IntMap Int
        sparseW' = W.fromList sElemsSearch :: W.IntMap Int
    evaluate $ rnf [denseM, sparseM, sparseM']
    evaluate $ rnf [denseW, sparseW, sparseW']
    evaluate $ rnf [elems,  sElems,  sElemsSearch]
    evaluate $ rnf [keys,   sKeys,   sKeysSearch]
    evaluate $ rnf [values, sValues]
    defaultMain
        [ bgroup "lookup"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (W.lookup k m)) 0 keys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n k -> fromMaybe n (W.lookup k m)) 0 sKeysSearch) sparseW
                ]
            ]
        , bgroup "member"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) 0 keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if W.member x m then n + 1 else n) 0 keys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\n x -> if M.member x m then n + 1 else n) 0 sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\n x -> if W.member x m then n + 1 else n) 0 sKeysSearch) sparseW
                ]
            ]
        , bgroup "insert"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m elems) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> W.insert k v m) m elems) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m -> foldl' (\m (k, v) -> M.insert k v m) m sElemsSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> W.insert k v m) m sElemsSearch) sparseW
                ]
            ]
        , bgroup "insertWith"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m (k, v) -> M.insertWith (+) k v m) m elems) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> W.insertWith (+) k v m) m elems) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m -> foldl' (\m (k, v) -> M.insertWith (+) k v m) m sElemsSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m (k, v) -> W.insertWith (+) k v m) m sElemsSearch) sparseW
                ]
            ]
        , bgroup "map"
            [ bench "IntMap"  $ whnf (M.map (+1)) denseM
            , bench "WordMap" $ whnf (W.map (+1)) denseW
            ]
        , bgroup "mapWithKey"
            [ bench "IntMap"  $ whnf (M.mapWithKey (+)) denseM
            , bench "WordMap" $ whnf (W.mapWithKey (+)) denseW
            ]
        , bgroup "foldlWithKey"
            [ bench "IntMap"  $ whnf (M.foldlWithKey add3 0) denseM
            , bench "WordMap" $ whnf (W.foldlWithKey add3 0) denseW
            ]
        , bgroup "foldlWithKey'"
            [ bench "IntMap"  $ whnf (M.foldlWithKey' add3 0) denseM
            , bench "WordMap" $ whnf (W.foldlWithKey' add3 0) denseW
            ]
        , bgroup "mapMaybe"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (M.mapMaybe Just) denseM
                , bench "WordMap" $ whnf (W.mapMaybe Just) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (M.mapMaybe (const Nothing)) denseM
                , bench "WordMap" $ whnf (W.mapMaybe (const Nothing)) denseW
                ]
            ]
        , bgroup "mapMaybeWithKey"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (M.mapMaybeWithKey (const Just)) denseM
                , bench "WordMap" $ whnf (W.mapMaybeWithKey (const Just)) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (M.mapMaybeWithKey (const (const Nothing))) denseM
                , bench "WordMap" $ whnf (W.mapMaybeWithKey (const (const Nothing))) denseW
                ]
            ]
        , bgroup "delete"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m k -> M.delete k m) m keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m k -> W.delete k m) m keys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap" $ whnf (\m -> foldl' (\m k -> M.delete k m) m sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m k -> W.delete k m) m sKeysSearch) sparseW
                ]
            ]
        , bgroup "update"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m k -> M.update Just k m) m keys) denseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m k -> W.update Just k m) m keys) denseW
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (\m -> foldl' (\m k -> M.update Just k m) m sKeysSearch) sparseM
                , bench "WordMap" $ whnf (\m -> foldl' (\m k -> W.update Just k m) m sKeysSearch) sparseW
                ]
            ]
        , bgroup "union"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (uncurry M.union) (denseM, sparseM)
                , bench "WordMap" $ whnf (uncurry W.union) (denseW, sparseW)
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (uncurry M.union) (sparseM, sparseM')
                , bench "WordMap" $ whnf (uncurry W.union) (sparseW, sparseW')
                ]
            ]
        , bgroup "unionWithKey"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (uncurry (M.unionWithKey (\k v1 v2 -> k + v1 + v2))) (denseM, sparseM)
                , bench "WordMap" $ whnf (uncurry (W.unionWithKey (\k v1 v2 -> k + v1 + v2))) (denseW, sparseW)
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (uncurry (M.unionWithKey (\k v1 v2 -> k + v1 + v2))) (sparseM, sparseM')
                , bench "WordMap" $ whnf (uncurry (W.unionWithKey (\k v1 v2 -> k + v1 + v2))) (sparseW, sparseW')
                ]
            ]
        , bgroup "difference"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (uncurry M.difference) (denseM, sparseM)
                , bench "WordMap" $ whnf (uncurry W.difference) (denseW, sparseW)
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (uncurry M.difference) (sparseM, sparseM')
                , bench "WordMap" $ whnf (uncurry W.difference) (sparseW, sparseW')
                ]
            ]
        , bgroup "intersection"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (uncurry M.intersection) (denseM, sparseM)
                , bench "WordMap" $ whnf (uncurry W.intersection) (denseW, sparseW)
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (uncurry M.intersection) (sparseM, sparseM')
                , bench "WordMap" $ whnf (uncurry W.intersection) (sparseW, sparseW')
                ]
            ]
        , bgroup "intersectionWithKey"
            [ bgroup "present"
                [ bench "IntMap"  $ whnf (uncurry (M.intersectionWithKey (\k v1 v2 -> k + v1 + v2))) (denseM, sparseM)
                , bench "WordMap" $ whnf (uncurry (W.intersectionWithKey (\k v1 v2 -> k + v1 + v2))) (denseW, sparseW)
                ]
            , bgroup "absent"
                [ bench "IntMap"  $ whnf (uncurry (M.intersectionWithKey (\k v1 v2 -> k + v1 + v2))) (sparseM, sparseM')
                , bench "WordMap" $ whnf (uncurry (W.intersectionWithKey (\k v1 v2 -> k + v1 + v2))) (sparseW, sparseW')
                ]
            ]
        , bgroup "fromList"
            [ bench "IntMap"  $ whnf M.fromList elems
            , bench "WordMap" $ whnf W.fromList elems
            ]
        , bgroup "fromAscList"
            [ bench "IntMap"  $ whnf M.fromAscList elems
            , bench "WordMap" $ whnf W.fromAscList elems
            ]
        ]
  where
    elems = zip keys values
    keys = [1..2^12]
    values = [1..2^12]
    sElems = zip sKeys sValues
    sElemsSearch = zip sKeysSearch sValues
    sKeys = [1,3..2^12]
    sKeysSearch = [2,4..2^12]
    sValues = [1,3..2^12]

{-# INLINE add3 #-}
add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c
