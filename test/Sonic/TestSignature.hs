{-# LANGUAGE TypeApplications #-}
module Sonic.TestSignature where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Crypto.Number.Generate (generateMax, generateBetween)

import Bulletproofs.Fq
import Bulletproofs.ArithmeticCircuit
import Sonic.Constraints
import Sonic.Signature
import Sonic.Utils as Utils
import qualified Sonic.SRS as SRS

test_signatures_of_computation :: TestTree
test_signatures_of_computation
  = localOption (QuickCheckTests 20)
    $ testProperty "Signatures of computation" $ QCM.monadicIO $ do
        x <- QCM.run (Utils.random @(PF Fq))
        alpha <- QCM.run (Utils.random @(PF Fq))
        d <- QCM.run (generateBetween 2 100)
        max <- QCM.run (generateBetween (d `quot` 2) (2*d-1))

        let bL0 :: Fq = 7
            bR0 :: Fq = 3
            bL1 :: Fq = 2

        let wL = [[1]
                 ,[0]]
            wR = [[0]
                 ,[1]]
            wO = [[0]
                 ,[0]]
            cs = [bL0 + bR0, bL1 + 10]
            aL = [10]
            aR = [12]
            aO = aL `hadamardp` aR
            gateWeights = GateWeights wL wR wO
            gateInputs = Assignment aL aR aO
            arithCircuit = ArithCircuit gateWeights [] cs

        let srs = SRS.new d x alpha
            m = length wL

        ys <- QCM.run $ replicateM m Utils.random
        proof <- QCM.run $ hscP srs gateWeights ys

        QCM.assert $ hscV srs ys gateWeights proof
