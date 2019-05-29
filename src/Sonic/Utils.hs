{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Sonic.Utils where

import Protolude
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
import qualified Data.List as L
import qualified Math.Polynomial as Poly
import Math.Polynomial.Laurent
import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Pairing.CyclicGroup (AsInteger(..))
import Pairing.Params (_r)

import Pairing.Group as Group (G1, G2, GT, g1, g2, expn)
import Pairing.Fr as Fr (Fr, random, frInv, new)

-- | Hadamard product or entry wise multiplication of two vectors
hadamardp :: Num a => [a] -> [a] -> [a]
hadamardp a b | length a == length b = zipWith (*) a b
              | otherwise = panic "Vector sizes must match"

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum $ hadamardp xs ys

(^+^) :: Num a => [a] -> [a] -> [a]
(^+^) = zipWith (+)

(^-^) :: Num a => [a] -> [a] -> [a]
(^-^) = zipWith (-)

evalOnY :: (Num f, Eq f, Fractional f) => f -> Laurent (Laurent f) -> Laurent f
evalOnY y l
  = newLaurent (expLaurent l) ((\l' -> evalLaurent l' y) <$> (coeffsLaurent l))

evalOnX :: (Num f, Eq f, Fractional f) => f -> Laurent (Laurent f) -> Laurent f
evalOnX x l
  = sum $ zipWith f [expLaurent l ..] (coeffsLaurent l)
  where
    f ex lau
      | ex >= 0 = lau `multLaurent` (newLaurent 0 [x ^ ex])
      | otherwise = lau `multLaurent` (newLaurent 0 [recip x ^ (abs ex)])

-- f(X) -> f(X, Y)
convertToTwoVariateX :: (Num f, Eq f) => Laurent f -> Laurent (Laurent f)
convertToTwoVariateX l
  = newLaurent (expLaurent l) ((\e -> newLaurent 0 [e]) <$> (coeffsLaurent l))

-- f(Y) -> f(X, Y)
convertToTwoVariateY :: (Num f, Eq f) => Laurent f -> Laurent (Laurent f)
convertToTwoVariateY l
  = newLaurent 0 [l]

random :: (Num f, AsInteger f, MonadRandom m) => m f
random = fromInteger <$> generateMax _r
