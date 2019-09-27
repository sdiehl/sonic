-- The helper protocol for computing aggregated signatures of correct computation.

{-# LANGUAGE RecordWildCards #-}
module Sonic.Signature
  ( HscProof(..)
  , hscP
  , hscV
  ) where

import Protolude
import Bulletproofs.ArithmeticCircuit (GateWeights(..))
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (rnd)
import Data.Pairing.BLS12381 (Fr, G1, BLS12381)
import Math.Polynomial.Laurent (evalLaurent)

import Sonic.Utils (evalOnX, evalOnY)
import Sonic.Constraints (sPoly)
import Sonic.CommitmentScheme (commitPoly, openPoly, pcV)
import Sonic.SRS (SRS(..))

data HscProof f = HscProof
  { hscS :: [G1 BLS12381]
  , hscW :: [(f, G1 BLS12381)]
  , hscQ :: [(f, G1 BLS12381)]
  , hscQz :: G1 BLS12381
  , hscC :: G1 BLS12381
  , hscU :: f
  , hscZ :: f
  }

hscP
  :: (MonadRandom m)
  => SRS
  -> GateWeights Fr
  -> [Fr]
  -> m (HscProof Fr)
hscP srs@SRS{..} weights ys = do
  let ss = commitPoly srs srsD . flip evalOnY (sPoly weights) <$> ys
  -- Random oracle
  u <- rnd
  let suX = evalOnX u (sPoly weights)
      commit = commitPoly srs srsD suX
      sW = zipWith (\yi si -> openPoly srs si u (evalOnY yi (sPoly weights))) ys ss
      sQ = (\yi -> openPoly srs commit yi suX) <$> ys
  -- Random oracle
  z <- rnd
  let (_suz, qz) = openPoly srs commit z suX
  pure HscProof
          { hscS = ss
          , hscW = sW
          , hscQ = sQ
          , hscQz = qz
          , hscC = commit
          , hscU = u
          , hscZ = z
          }

hscV
  :: SRS
  -> [Fr]
  -> GateWeights Fr
  -> HscProof Fr
  -> Bool
hscV srs@SRS{..} ys weights proof@HscProof{..}
  = let sz = evalLaurent (evalOnY hscZ (sPoly weights)) hscU
    in and
        $ pcV srs srsD hscC hscZ (sz, hscQz)
        : (zipWith (flip (pcV srs srsD) hscU) hscS hscW
        ++ zipWith (pcV srs srsD hscC) ys hscQ)
