module Sonic.SRS where

import Protolude
import Pairing.Group as Group (G1, G2, GT, g1, g2, expn)
import Pairing.Fr as Fr (Fr(..))
import Pairing.Pairing (reducedPairing)
import PrimeField
data SRS = SRS
  { d :: Integer
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

new :: (KnownNat p) => Integer -> PrimeField p -> PrimeField p -> SRS
new d x alpha
  = let xInv = recip x
    in SRS
        { d = d
        , gNegativeX = (\i -> expn g1 (xInv ^ i)) <$> [1..d]
        , gPositiveX = (\i -> expn g1 (x ^ i)) <$> [0..d]
        , hNegativeX = (\i -> expn g2 (xInv ^ i)) <$> [1..d]
        , hPositiveX = (\i -> expn g2 (x ^ i)) <$> [0..d]
        , gNegativeAlphaX = (\i -> expn g1 (alpha * (xInv ^ i))) <$> [1..d]
        -- g^alpha is not shared
        , gPositiveAlphaX = (\i -> expn g1 (alpha * (x ^ i))) <$> [1..d]
        , hNegativeAlphaX = (\i -> expn g2 (alpha * (xInv ^ i))) <$> [1..d]
        , hPositiveAlphaX = (\i -> expn g2 (alpha * (x ^ i))) <$> [0..d]
        , srsPairing = reducedPairing g1 (expn g2 alpha)
        , gPositiveAlphaX' = (\i -> expn g1 (alpha * (x ^ i))) <$> [0..d]
        }

