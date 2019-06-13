{-# LANGUAGE RecordWildCards #-}
module Sonic.Protocol where

import Protolude hiding (head)
import Data.List (head)
import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)
import Pairing.Group as Group (G1, G2, GT, g1, g2, expn)
import Pairing.CyclicGroup (AsInteger(..))
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent

import Text.PrettyPrint.Leijen.Text as PP (pretty, Pretty(..))
import Sonic.SRS
import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Signature
import Sonic.Utils as Utils

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
  , prt' :: f
  }

prover
  :: (Show f, Num f, Eq f, Fractional f, AsInteger f, MonadRandom m)
  => SRS
  -> Assignment f
  -> ArithCircuit f
  -> f
  -> m (Proof f, f, f, [f])
prover srs assignment@Assignment{..} arithCircuit@ArithCircuit{..} x = do
  cns <- replicateM 4 Utils.random
  let rXY = rPoly assignment
      sumcXY = newLaurent
                (negate (2 * n + 4))
                (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
      polyR' = addLaurent rXY sumcXY
      commitR = commitPoly srs (fromIntegral n) (evalOnY 1 polyR') x
  -- zkV -> zkP: Send y to prover
  -- (Random oracle)
  y <- Utils.random
  let ky = polyK cs n
  let sP = sPoly weights
  let tP = tPoly polyR' sP ky
      tPY = evalOnY y tP

  let commitT = commitPoly srs (d srs) tPY x
  -- zkV -> zkP: Send y to prover
  -- (Random oracle)
  z <- Utils.random
  let (a, wa) = openPoly srs commitR z (evalOnY 1 polyR') x
      (b, wb) = openPoly srs commitR (y * z) (evalOnY 1 polyR') x
      (t', wt) = openPoly srs commitT z (evalOnY y tP) x

  let s = evalLaurent (evalOnY y sP) z
  ys <- replicateM m Utils.random
  hscProof <- hscP srs weights ys x
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
  :: (Num f, Eq f, Fractional f, AsInteger f)
  => SRS
  -> ArithCircuit f
  -> Proof f
  -> f
  -> f
  -> [f]
  -> Bool
verifier srs@SRS{..} ArithCircuit{..} Proof{..} y z ys
  = let t = (prA * (prB + prS)) + (negate $ evalLaurent ky y)
        checks = [ hscV srs ys weights prHscProof
                 , pcV srs (fromIntegral n) prR z (prA, prWa)
                 , pcV srs (fromIntegral n) prR (y * z) (prB, prWb)
                 , pcV srs d prT z (t, prWt)
                ]
    in and $ traceShow checks checks
  where
    n = length . head . wL $ weights
    ky = polyK cs n
