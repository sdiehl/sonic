-- The interactive Sonic protocol to check that the prover knows a valid assignment of the wires in the circuit

{-# LANGUAGE RecordWildCards #-}
module Sonic.Protocol
  ( Proof
  , prove
  , verify
  ) where

import Protolude hiding (head)
import Data.List (head)
import Data.Pairing.BLS12381 (Fr, G1, BLS12381)
import Control.Monad.Random (MonadRandom)
import Bulletproofs.ArithmeticCircuit (ArithCircuit(..), Assignment(..), GateWeights(..))
import Math.Polynomial.Laurent (newLaurent, evalLaurent)
import Data.Field.Galois (rnd)

import Sonic.SRS (SRS(..))
import Sonic.Constraints (rPoly, sPoly, tPoly, kPoly)
import Sonic.CommitmentScheme (commitPoly, openPoly, pcV)
import Sonic.Signature (HscProof(..), hscP, hscV)
import Sonic.Utils (evalOnY)

data Proof f = Proof
  { prR :: G1 BLS12381
  , prT :: G1 BLS12381
  , prA :: f
  , prWa :: G1 BLS12381
  , prB :: f
  , prWb :: G1 BLS12381
  , prWt :: G1 BLS12381
  , prS :: f
  , prHscProof :: HscProof f
  }

prove
  :: MonadRandom m
  => SRS
  -> Assignment Fr
  -> ArithCircuit Fr
  -> m (Proof Fr, Fr, Fr, [Fr])
prove srs@SRS{..} assignment@Assignment{..} arithCircuit@ArithCircuit{..} =
  if srsD < 7*n
    then panic $ "Parameter d is not large enough: " <> show srsD <> " should be greater than " <>  show (7*n)
    else do
    cns <- replicateM 4 rnd
    let rXY = rPoly assignment
        sumcXY = newLaurent
                 (negate (2 * n + 4))
                 (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
        polyR' = rXY + sumcXY
        commitR = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')

    -- zkV -> zkP: Send y to prover (Random oracle)
    y <- rnd
    let ky = kPoly cs n
    let sP = sPoly weights
    let tP = tPoly polyR' sP ky
        tPY = evalOnY y tP

    let commitT = commitPoly srs srsD tPY

    -- zkV -> zkP: Send y to prover (Random oracle)
    z <- rnd
    let (a, wa) = openPoly srs commitR z (evalOnY 1 polyR')
        (b, wb) = openPoly srs commitR (y * z) (evalOnY 1 polyR')
        (_, wt) = openPoly srs commitT z (evalOnY y tP)

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

verify
  :: SRS
  -> ArithCircuit Fr
  -> Proof Fr
  -> Fr
  -> Fr
  -> [Fr]
  -> Bool
verify srs@SRS{..} ArithCircuit{..} Proof{..} y z ys
  = let t = prA * (prB + prS) - evalLaurent ky y
        checks = [ hscV srs ys weights prHscProof
                 , pcV srs (fromIntegral n) prR z (prA, prWa)
                 , pcV srs (fromIntegral n) prR (y * z) (prB, prWb)
                 , pcV srs srsD prT z (t, prWt)
                ]
    in and checks
  where
    n = length . head . wL $ weights
    ky = kPoly cs n
