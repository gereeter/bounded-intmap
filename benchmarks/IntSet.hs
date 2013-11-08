{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Criterion.Main
import Data.List (foldl')
import qualified Data.IntSet as S
import qualified Data.WordSet as W
import Data.Word (Word)

main = do
    let s = S.fromAscList elems :: S.IntSet
        s_even = S.fromAscList elems_even :: S.IntSet
        s_odd = S.fromAscList elems_odd :: S.IntSet
        w = W.fromAscList elemsW :: W.WordSet
        w_even = W.fromAscList elemsW_even :: W.WordSet
        w_odd = W.fromAscList elemsW_odd :: W.WordSet
    evaluate $ rnf [s, s_even, s_odd]
    evaluate $ rnf [w, w_even, w_odd]
    defaultMain
        [ bgroup "member"
            [ bench "IntSet"  $ whnf (member elems) s
            , bench "WordSet" $ whnf (memberW elemsW) w
            ]
        , bgroup "insert"
            [ bench "IntSet"  $ whnf (ins elems) S.empty
            , bench "WordSet" $ whnf (insW elemsW) W.empty
            ]
        , bgroup "map"
            [ bench "IntSet"  $ whnf (S.map (+ 1)) s
            , bench "WordSet" $ whnf (W.map (+ 1)) w
            ]
        , bgroup "filter"
            [ bench "IntSet"  $ whnf (S.filter ((== 0) . (`mod` 2))) s
            , bench "WordSet" $ whnf (W.filter ((== 0) . (`mod` 2))) w
            ]
        , bgroup "partition"
            [ bench "IntSet"  $ nf (S.partition ((== 0) . (`mod` 2))) s
            , bench "WordSet" $ nf (W.partition ((== 0) . (`mod` 2))) w
            ]
        , bgroup "foldr"
            [ bench "IntSet"  $ whnf (S.foldr (+) 0) s
            , bench "WordSet" $ whnf (W.foldr (+) 0) w
            ]
        , bgroup "delete"
            [ bench "IntSet"  $ whnf (del elems) s
            , bench "WordSet" $ whnf (delW elemsW) w
            ]
        , bgroup "findMin"
            [ bench "IntSet"  $ whnf S.findMin s
            , bench "WordSet" $ whnf W.findMin w
            ]
        , bgroup "findMax"
            [ bench "IntSet"  $ whnf S.findMax s
            , bench "WordSet" $ whnf W.findMax w
            ]
        , bgroup "deleteMin"
            [ bench "IntSet"  $ whnf S.deleteMin s
            , bench "WordSet" $ whnf W.deleteMin w
            ]
        , bgroup "deleteMax"
            [ bench "IntSet"  $ whnf S.deleteMax s
            , bench "WordSet" $ whnf W.deleteMax w
            ]
        , bgroup "unions"
            [ bench "IntSet"  $ whnf S.unions [s_even, s_odd]
            , bench "WordSet" $ whnf W.unions [w_even, w_odd]
            ]
        , bgroup "union"
            [ bench "IntSet"  $ whnf (S.union s_even) s_odd
            , bench "WordSet" $ whnf (W.union w_even) w_odd
            ]
        , bgroup "difference"
            [ bench "IntSet"  $ whnf (S.difference s) s_even
            , bench "WordSet" $ whnf (W.difference w) w_even
            ]
        , bgroup "intersection"
            [ bench "IntSet"  $ whnf (S.intersection s) s_even
            , bench "WordSet" $ whnf (W.intersection w) w_even
            ]
        , bgroup "fromList"
            [ bench "IntSet"  $ whnf S.fromList elems
            , bench "WordSet" $ whnf W.fromList elemsW
            ]
        , bgroup "fromAscList"
            [ bench "IntSet"  $ whnf S.fromAscList elems
            , bench "WordSet" $ whnf W.fromAscList elemsW
            ]
        , bgroup "fromDistinctAscList"
            [ bench "IntSet"  $ whnf S.fromDistinctAscList elems
            , bench "WordSet" $ whnf W.fromDistinctAscList elemsW
            ]
        ]
  where
    elems = [1..2^12]
    elems_even = [2,4..2^12]
    elems_odd = [1,3..2^12]
    elemsW = [1..2^12]
    elemsW_even = [2,4..2^12]
    elemsW_odd = [1,3..2^12]

member :: [Int] -> S.IntSet -> Int
member xs s = foldl' (\n x -> if S.member x s then n + 1 else n) 0 xs

memberW :: [Word] -> W.WordSet -> Int
memberW xs s = foldl' (\n x -> if W.member x s then n + 1 else n) 0 xs

ins :: [Int] -> S.IntSet -> S.IntSet
ins xs s0 = foldl' (\s a -> S.insert a s) s0 xs

insW :: [Word] -> W.WordSet -> W.WordSet
insW xs s0 = foldl' (\s a -> W.insert a s) s0 xs

del :: [Int] -> S.IntSet -> S.IntSet
del xs s0 = foldl' (\s k -> S.delete k s) s0 xs

delW :: [Word] -> W.WordSet -> W.WordSet
delW xs s0 = foldl' (\s k -> W.delete k s) s0 xs
