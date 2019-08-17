{-# LANGUAGE RecordWildCards #-}
module Sonic.Protocol where

import Protolude hiding (head)
import Data.List (head)
import Control.Monad.Random (MonadRandom)
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent
import GaloisField (GaloisField(rnd))

-- import Text.PrettyPrint.Leijen.Text as PP (pretty, Pretty(..))
import Sonic.SRS
import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Signature
import Sonic.Utils
import Sonic.Curve (Fr, G1, G2, GT)

data Proof f = Proof
  { prR :: G1
  , prT :: G1
  , prA :: f
  , prWa :: G1
  , prB :: f
  , prWb :: G1
  , prWt :: G1
  , prS :: f
  , prHscProof :: HscProof f
  }

prover
  :: MonadRandom m
  => SRS
  -> Assignment Fr
  -> ArithCircuit Fr
  -> m (Proof Fr, Fr, Fr, [Fr])
prover srs@SRS{..} assignment@Assignment{..} arithCircuit@ArithCircuit{..} = do
  cns <- replicateM 4 rnd
  let rXY = rPoly assignment
      sumcXY = newLaurent
                (negate (2 * n + 4))
                (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
      polyR' = addLaurent rXY sumcXY
      commitR = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')
  -- zkV -> zkP: Send y to prover
  -- (Random oracle)
  y <- rnd
  let ky = kPoly cs n
  let sP = sPoly weights
  let tP = tPoly polyR' sP ky
      tPY = evalOnY y tP

  let commitT = commitPoly srs srsD tPY
  -- zkV -> zkP: Send y to prover
  -- (Random oracle)
  z <- rnd
  let (a, wa) = openPoly srs commitR z (evalOnY 1 polyR')
      (b, wb) = openPoly srs commitR (y * z) (evalOnY 1 polyR')
      (t', wt) = openPoly srs commitT z (evalOnY y tP)

  let s = evalLaurent (evalOnY y sP) z
  ys <- replicateM m rnd
  hscProof <- hscP srs weights ys
  pure ( Proof
          { prR = commitR
          , prT = commitT
          , prA = a
          , prWa = wa
          , prB = b
          , prWb = wb
          , prWt = wt
          , prS = s
          , prHscProof = hscProof
          }
       , y
       , z
       , ys
       )
  where
    n :: Int
    n = length aL
    m :: Int
    m = length . wL $ weights

verifier
  :: SRS
  -> ArithCircuit Fr
  -> Proof Fr
  -> Fr
  -> Fr
  -> [Fr]
  -> Bool
verifier srs@SRS{..} ArithCircuit{..} Proof{..} y z ys
  = let t = prA * (prB + prS) - evalLaurent ky y
        checks = [ hscV srs ys weights prHscProof
                 , pcV srs (fromIntegral n) prR z (prA, prWa)
                 , pcV srs (fromIntegral n) prR (y * z) (prB, prWb)
                 , pcV srs srsD prT z (t, prWt)
                ]
    in traceShow checks $ and checks
  where
    n = length . head . wL $ weights
    ky = kPoly cs n
