{-# LANGUAGE RecordWildCards #-}
-- Signature of correct computation
module Sonic.Signature where

import Protolude
import Sonic.SRS
import Control.Monad.Random (MonadRandom)

import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent
import GaloisField (GaloisField(rnd))

import Sonic.Utils as Utils
import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Curve (Fr, G1)

data HscProof f = HscProof
  { hscS :: [G1]
  , hscW :: [(f, G1)]
  , hscQ :: [(f, G1)]
  , hscQz :: G1
  , hscC :: G1
  , hscU :: f
  , hscZ :: f
  }

-- Helper protocol
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
  let (suz, qz) = openPoly srs commit z suX
  pure HscProof
          { hscS = ss
          , hscW = sW
          , hscQ = sQ
          , hscQz = qz
          , hscC = commit
          , hscU = u
          , hscZ = z
          }
  where
    m = length ys

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
        : (zipWith (\sj (wsj, wj) -> pcV srs srsD sj hscU (wsj, wj)) hscS hscW
        ++ zipWith (\yj (wsj, wj) -> pcV srs srsD hscC yj (wsj, wj)) ys hscQ)




