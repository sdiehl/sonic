{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Sonic.Utils
  ( BiVariateLaurent
  , hadamardp
  , dot
  , evalOnX
  , evalOnY
  , convertToTwoVariateX
  , convertToTwoVariateY
  ) where

import Protolude
import Math.Polynomial.Laurent

import Sonic.Curve (Fr)

type BiVariateLaurent f = Laurent (Laurent f)

-- | Hadamard product or entry wise multiplication of two vectors
hadamardp :: Num a => [a] -> [a] -> [a]
hadamardp a b | length a == length b = zipWith (*) a b
              | otherwise = panic "Vector sizes must match"

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum $ hadamardp xs ys

evalOnY :: Fr -> BiVariateLaurent Fr -> Laurent Fr
evalOnY y l
  = newLaurent (expLaurent l) ((\l' -> evalLaurent l' y) <$> (coeffsLaurent l))

evalOnX :: Fr -> BiVariateLaurent Fr -> Laurent Fr
evalOnX x l
  = sum $ zipWith f [expLaurent l ..] (coeffsLaurent l)
  where
    f ex lau
      | ex >= 0 = lau `multLaurent` (newLaurent 0 [x ^ ex])
      | otherwise = lau `multLaurent` (newLaurent 0 [recip x ^ (abs ex)])

-- f(X) -> f(X, 0)
convertToTwoVariateX :: (Num f, Eq f) => Laurent f -> BiVariateLaurent f
convertToTwoVariateX l
  = newLaurent (expLaurent l) ((\e -> newLaurent 0 [e]) <$> (coeffsLaurent l))

-- f(Y) -> f(0, Y)
convertToTwoVariateY :: (Num f, Eq f) => Laurent f -> BiVariateLaurent f
convertToTwoVariateY l = newLaurent 0 [l]

