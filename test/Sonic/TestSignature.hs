{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Sonic.TestSignature where

import Protolude
import Control.Monad.Random (getRandomR)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import Bulletproofs.ArithmeticCircuit (ArithCircuit(..), GateWeights(..), Assignment(..))
import GaloisField(GaloisField(rnd))

import Sonic.Signature
import Sonic.Utils
import Sonic.Curve (Fr)
import qualified Sonic.SRS as SRS
import Sonic.Reference

test_signatures_of_computation :: TestTree
test_signatures_of_computation
  = localOption (QuickCheckTests 20)
    $ testProperty "Signatures of computation" $ QCM.monadicIO $ do
        x <- QCM.run rnd
        z <- QCM.run rnd
        alpha <- QCM.run rnd
        d <- QCM.run (getRandomR (2, 100))
        acExample <- QCM.run . generate $ oneof
          [ pure $ arithCircuitExample1 x z
          , pure $ arithCircuitExample2 x z
          ]
        let arithCircuit@ArithCircuit{..} = aceCircuit acExample
            assignment@Assignment{..} = aceAssignment acExample
        let srs = SRS.new d x alpha
            m = length $ wL weights
        ys <- QCM.run $ replicateM m rnd
        proof <- QCM.run $ hscP srs weights ys
        QCM.assert $ hscV srs ys weights proof
