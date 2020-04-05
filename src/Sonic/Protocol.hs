-- The interactive Sonic protocol to check that the prover knows a valid assignment of the wires in the circuit

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Sonic.Protocol
  ( Proof
  , RndOracle(..)
  , prove
  , verify
  ) where

import Protolude hiding (head)
import Data.List (head)
import Data.Pairing.BLS12381 (Fr, G1, BLS12381)
import Control.Monad.Random (MonadRandom)
import Bulletproofs.ArithmeticCircuit (ArithCircuit(..), Assignment(..), GateWeights(..))
import Data.Field.Galois (rnd)
import Data.Poly.Sparse.Laurent (monomial, eval)
import qualified GHC.Exts

import Sonic.SRS (SRS(..))
import Sonic.Constraints (rPoly, sPoly, tPoly, kPoly)
import Sonic.CommitmentScheme (commitPoly, openPoly, pcV)
import Sonic.Signature (HscProof(..), hscProve, hscVerify)
import Sonic.Utils (evalY, BiVLaurent)

data Proof = Proof
  { prR :: G1 BLS12381
  , prT :: G1 BLS12381
  , prA :: Fr
  , prWa :: G1 BLS12381
  , prB :: Fr
  , prWb :: G1 BLS12381
  , prWt :: G1 BLS12381
  , prS :: Fr
  , prHscProof :: HscProof
  } deriving (Eq, Show, Generic, NFData)

-- | Values created non-interactively in the random oracle model during proof generation
data RndOracle = RndOracle
  { rndOracleY :: Fr
  , rndOracleZ :: Fr
  , rndOracleYZs :: [(Fr, Fr)]
  } deriving (Eq, Show, Generic, NFData)

prove
  :: MonadRandom m
  => SRS
  -> Assignment Fr
  -> ArithCircuit Fr
  -> m (Proof, RndOracle)
prove srs@SRS{..} assignment@Assignment{..} arithCircuit@ArithCircuit{..} =
  if srsD < 7*n
    then panic $ "Parameter d is not large enough: " <> show srsD <> " should be greater than " <>  show (7*n)
    else do
    -- zkP_1(info,a,b,c) -> R
    cns <- replicateM 4 rnd                 -- c_{n+1}, c_{n+2}, c_{n+3}, c_{n+4} <- F_p
    let sumcXY :: BiVLaurent Fr             -- \sum_{i=1}^4 c_{n+i}X^{-2n-i}Y^{-2n-i}
        sumcXY = GHC.Exts.fromList $
          zipWith (\i cni -> (negate (2 * n + i), monomial (negate (2 * n + i)) cni)) [1..] cns
        polyR' = rPoly assignment + sumcXY  -- r(X, Y) <- r(X, Y) + \sum_{i=1}^4 c_{n+i}X^{-2n-i}Y^{-2n-i}
        commitR = commitPoly srs (fromIntegral n) (evalY 1 polyR') -- R <- Commit(bp,srs,n,r(X,1))

    -- zkV_1(info, R) -> y
    y <- rnd

    -- zkP_2(y) -> T
    let kY = kPoly cs n                     -- k(Y)
        sXY = sPoly weights                 -- s(X, Y)
        tXY = tPoly polyR' sXY kY           -- t(X, Y)
        tXy = evalY y tXY                   -- t(X, y)
        commitT = commitPoly srs srsD tXy   -- T

    -- zkV_2(T) -> z
    z <- rnd

    -- zkP_3(z) -> (a, W_a, b, W_b_, W_t, s, sc)
    let (a, wa) = openPoly srs z (evalY 1 polyR')        -- (a=r(z,1),W_a) <- Open(R,z,r(X,1))
        (b, wb) = openPoly srs (y * z) (evalY 1 polyR')  -- (b=r(z,y),W_b) <- Open(R,yz,r(X,1))
        (_, wt) = openPoly srs z (evalY y tXY)            -- (a=r(z,1),W_a) <- Open(T,z,t(X,y))

    let szy = eval (evalY y sXY) z                        -- s=s(z,y)
    ys <- replicateM m rnd
    zs <- replicateM m rnd
    let yzs = zip ys zs
    hscProof <- hscProve srs sXY yzs
    pure ( Proof
           { prR = commitR
           , prT = commitT
           , prA = a
           , prWa = wa
           , prB = b
           , prWb = wb
           , prWt = wt
           , prS = szy
           , prHscProof = hscProof
           }
         , RndOracle
           { rndOracleY = y
           , rndOracleZ = z
           , rndOracleYZs = yzs
           }
         )
  where
    n :: Int
    n = length aL
    m :: Int
    m = length . wL $ weights

verify
  :: SRS
  -> ArithCircuit Fr
  -> Proof
  -> Fr
  -> Fr
  -> [(Fr, Fr)]
  -> Bool
verify srs@SRS{..} ArithCircuit{..} Proof{..} y z yzs
  = let t = prA * (prB + prS) - eval kY y
        checks = [ hscVerify srs sXY yzs prHscProof
                 , pcV srs (fromIntegral n) prR z (prA, prWa)
                 , pcV srs (fromIntegral n) prR (y * z) (prB, prWb)
                 , pcV srs srsD prT z (t, prWt)
                ]
    in and checks
  where
    n = length . head . wL $ weights
    kY = kPoly cs n
    sXY = sPoly weights
