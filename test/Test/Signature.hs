{-# LANGUAGE RecordWildCards #-}
module Test.Signature where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import Bulletproofs.ArithmeticCircuit (ArithCircuit(..), GateWeights(..), Assignment(..))
import Data.Field.Galois (rnd)

import Sonic.Signature
import qualified Sonic.SRS as SRS

import Test.Reference

-- (s=s(z,y),sc) ← scP(info,s(X,Y),(z,y))
-- check scV(info,s(X,Y),(z,y),(s,sc))
test_signatures_of_computation :: TestTree
test_signatures_of_computation
  = localOption (QuickCheckTests 20)
    $ testProperty "Signatures of computation" $ QCM.monadicIO $ do
        RandomParams{..} <- lift randomParams
        (ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
        let m = length $ wL weights
            n = length $ aL assignment

        d <- lift $ randomD n
        let srs = SRS.new d pX pAlpha
        ys <- lift $ replicateM m rnd
        proof <- lift $ hscP srs weights ys
        QCM.assert $ hscV srs ys weights proof
