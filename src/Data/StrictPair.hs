module Data.StrictPair where

data StrictPair a b = !a :*: !b

{-# INLINE toPair #-}
toPair :: StrictPair a b -> (a, b)
toPair (a :*: b) = (a, b)
