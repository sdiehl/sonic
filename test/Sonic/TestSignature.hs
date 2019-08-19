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
        RandomParams{..} <- lift randomParams
        (aCircuit@ArithCircuit{..}, assignment) <- lift . generate $ oneof
          [ pure $ arithCircuitExample1 pX pZ
          , pure $ arithCircuitExample2 pX pZ
          ]
        let m = length $ wL weights
            n = length $ aL assignment

        d <- lift $ randomD n
        let srs = SRS.new d pX pAlpha
        ys <- lift $ replicateM m rnd
        proof <- lift $ hscP srs weights ys
        QCM.assert $ hscV srs ys weights proof
