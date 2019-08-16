{-# LANGUAGE TypeApplications #-}

module Sonic.SRS where

import Protolude
import Sonic.Curve (Fr, G1, G2, GT, gG1, gG2)
import Pairing.Pairing (reducedPairing)
import Curve (Curve(..))

data SRS = SRS
  { srsD :: Integer
  , gNegativeX :: [G1]
  , gPositiveX :: [G1]
  , hNegativeX :: [G2]
  , hPositiveX :: [G2]
  , gNegativeAlphaX :: [G1]
  , gPositiveAlphaX :: [G1]
  , hNegativeAlphaX :: [G2]
  , hPositiveAlphaX :: [G2]
  , srsPairing :: GT
  , gPositiveAlphaX' :: [G1]
  }

new :: Integer -> Fr -> Fr -> SRS
new d x alpha
  = let xInv = recip x
    in SRS
        { srsD = d
        , gNegativeX = (mul gG1 . (^) xInv) <$> [1..d]
        , gPositiveX = (mul gG1 . (^) x) <$> [0..d]
        , hNegativeX = (mul gG2 . (^) xInv) <$> [1..d]
        , hPositiveX = (mul gG2 . (^) x) <$> [0..d]
        , gNegativeAlphaX = (mul gG1 . ((*) alpha . (^) xInv)) <$> [1..d]
        -- g^alpha is not shared
        , gPositiveAlphaX = (mul gG1 0) : (mul gG1 . ((*) alpha . (^) x) <$> [1..d])
        , hNegativeAlphaX = (mul gG2 . ((*) alpha . (^) xInv)) <$> [1..d]
        , hPositiveAlphaX = (mul gG2 . ((*) alpha . (^) x)) <$> [0..d]
        , srsPairing = reducedPairing gG1 (mul gG2 alpha)

        -- TODO: Remove this line. It's for testing purposes only
        , gPositiveAlphaX' = (mul gG1 . ((*) alpha . ((^) x))) <$> [0..d]
        }

