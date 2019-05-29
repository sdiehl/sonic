module Sonic.TestProtocol where

import Protolude
import Pairing.Fr as Fr (Fr(..), new, random)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Crypto.Number.Generate (generateMax, generateBetween)

import Bulletproofs.ArithmeticCircuit
import Sonic.Protocol
import Sonic.Utils
import qualified Sonic.SRS as SRS

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
test_sonic = localOption (QuickCheckTests 5)
  $ testProperty "Sonic protocol" $ QCM.monadicIO $ do
    x <- QCM.run Fr.random
    z <- QCM.run Fr.random
    alpha <- QCM.run Fr.random
    d <- QCM.run (generateBetween 2 100)

    let wL = [[0, 0]
             ,[1, 0]
             ,[0, 1]
             ,[0, 0]
             ,[0, 0]]
        wR = [[0, 0]
             ,[0, 0]
             ,[0, 0]
             ,[1, 0]
             ,[0, 1]]
        wO = [[1, -1]
             ,[0, 0]
             ,[0, 0]
             ,[0, 0]
             ,[0, 0]]
        wV = [[0, 0, 0, 0]
             ,[1, 0, 0, 0]
             ,[0, 0, 1, 0]
             ,[0, 1, 0 ,0]
             ,[0, 0, 0, 1]]
        cs = [0, -z, -z, -z, -z]
        aL = [4 - z, 9 - z]
        aR = [9 - z, 4 - z]
        aO = aL `hadamardp` aR
        vs = [4, 9, 9, 4]
        gateWeights = GateWeights wL wR wO
        gateInputs = Assignment aL aR aO
        arithCircuit = ArithCircuit gateWeights wV cs

    let srs = SRS.new d x alpha

    (proof, y, z, ys) <- QCM.run $ prover srs gateInputs arithCircuit x

    QCM.assert $ verifier srs arithCircuit proof y z ys
