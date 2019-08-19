{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Sonic.Utils
  ( BiVariateLaurent
  , evalOnX
  , evalOnY
  , convertToTwoVariateX
  , convertToTwoVariateY
  ) where

import Protolude
import Math.Polynomial.Laurent (Laurent, newLaurent, evalLaurent, expLaurent, coeffsLaurent)
import GaloisField (GaloisField(..))
import Sonic.Curve (Fr)

type BiVariateLaurent f = Laurent (Laurent f)

evalOnY :: Fr -> BiVariateLaurent Fr -> Laurent Fr
evalOnY y l
  = newLaurent (expLaurent l) ((\l' -> evalLaurent l' y) <$> (coeffsLaurent l))

evalOnX :: Fr -> BiVariateLaurent Fr -> Laurent Fr
evalOnX x l
  = sum $ zipWith f [expLaurent l ..] (coeffsLaurent l)
  where
    f ex lau = lau * (newLaurent 0 [x `pow` fromIntegral ex])

-- f(X) -> f(X, 0)
convertToTwoVariateX :: (Num f, Eq f) => Laurent f -> BiVariateLaurent f
convertToTwoVariateX l
  = newLaurent (expLaurent l) ((\e -> newLaurent 0 [e]) <$> (coeffsLaurent l))

-- f(Y) -> f(0, Y)
convertToTwoVariateY :: (Num f, Eq f) => Laurent f -> BiVariateLaurent f
convertToTwoVariateY l = newLaurent 0 [l]
