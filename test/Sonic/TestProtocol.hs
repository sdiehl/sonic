{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE RecordWildCards #-} 
module Sonic.TestProtocol where

import Protolude
import Control.Monad.Random (getRandomR)
import Data.List (zipWith4)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import Bulletproofs.ArithmeticCircuit

import Sonic.Protocol
import Sonic.Utils
import Sonic.SRS
import Sonic.Curve (Fr)
import qualified Sonic.SRS as SRS
import Sonic.Reference

--  5 linear constraints (q = 5):
--  aO[0] = aO[1]
--  aL[0] = V[0] - z
--  aL[1] = V[2] - z
--  aR[0] = V[1] - z
--  aR[1] = V[3] - z
--
--  2 multiplication constraint (implicit) (n = 2):
--  aL[0] * aR[0] = aO[0]
--  aL[1] * aR[1] = aO[1]
--
--  4 input values (m = 4)
test_sonic :: TestTree
test_sonic = localOption (QuickCheckTests 20)
  $ testProperty "Sonic protocol" $ QCM.monadicIO $ do
    RandomParams {..} <- lift randomParams

    let (arithCircuit, assignment@Assignment{..}) = arithCircuitExample2 pX pZ
        n = length aL
    d <- lift $ randomD n
    let srs = SRS.new d pX pAlpha
    (proof, y, z, ys) <- lift $ prover srs assignment arithCircuit
    QCM.assert $ verifier srs arithCircuit proof y z ys
