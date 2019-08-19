-- Structured reference string with unknowns x and α
-- This string is designed so that g^α is omitted from the reference string.
module Sonic.SRS where

import Protolude
import Pairing.Pairing (reducedPairing)
import Curve (Curve(..), Group(..))

import Sonic.Curve (Fr, G1, G2, GT)

data SRS = SRS
  { srsD :: Int
  , gNegativeX :: [G1]
  , gPositiveX :: [G1]
  , hNegativeX :: [G2]
  , hPositiveX :: [G2]
  , gNegativeAlphaX :: [G1]
  , gPositiveAlphaX :: [G1]
  , hNegativeAlphaX :: [G2]
  , hPositiveAlphaX :: [G2]
  , srsPairing :: GT
  }

-- | Create a new Structured Reference String (SRS)
-- 'd' should be large enough to support the circuit depth 'n'.
-- In this implementation, 'd' should be greater than (7 * 'n')
new :: Int -> Fr -> Fr -> SRS
new d x alpha
  = let xInv = recip x
    in SRS
        { srsD = d
        , gNegativeX = mul gen . (^) xInv <$> [1..d]
        , gPositiveX = mul gen . (^) x <$> [0..d]
        , hNegativeX = mul gen . (^) xInv <$> [1..d]
        , hPositiveX = mul gen . (^) x <$> [0..d]
        , gNegativeAlphaX = mul gen . ((*) alpha . (^) xInv) <$> [1..d]
        -- g^alpha is not shared
        , gPositiveAlphaX = mul gen 0 : (mul gen . ((*) alpha . (^) x) <$> [1..d])
        , hNegativeAlphaX = mul gen . ((*) alpha . (^) xInv) <$> [1..d]
        , hPositiveAlphaX = mul gen . ((*) alpha . (^) x) <$> [0..d]
        , srsPairing = reducedPairing gen (mul gen alpha)
        }
