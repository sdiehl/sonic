module Main where

import Protolude
import Crypto.Number.Generate (generateBetween)
import Bulletproofs.ArithmeticCircuit
import Pairing.Fr as Fr
import Pairing.CyclicGroup
import Sonic.SRS as SRS
import Sonic.Protocol as Protocol
import Sonic.Utils as Utils

sonicProtocol :: ArithCircuit Fr -> Assignment Fr -> Fr -> IO Bool
sonicProtocol circuit@(ArithCircuit gates wV cs) assignment x = do
  -- Setup for an SRS
  srs <- SRS.new <$> generateBetween 2 100 <*> pure x <*> Utils.random
  -- Prover
  (proof, y, z, ys) <- prover srs assignment circuit
  -- Verifier
  pure $ verifier srs circuit proof y z ys


--  Example:
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
runExample :: IO ()
runExample = do
  -- Arithmetic circuit
  z <- Utils.random
  let cs = [0, -z, -z, -z, -z]
      wV = [[0, 0, 0, 0]
           ,[1, 0, 0, 0]
           ,[0, 0, 1, 0]
           ,[0, 1, 0 ,0]
           ,[0, 0, 0, 1]]
      arithCircuit = ArithCircuit gateWeights wV cs

  -- Assignment
  let aL = [4 - z, 9 - z]
      aR = [9 - z, 4 - z]
      aO = aL `hadamardp` aR
      assignment = Assignment aL aR aO

  -- Run protocol
  print =<< sonicProtocol arithCircuit assignment =<< Utils.random

  where
    gateWeights :: GateWeights Fr
    gateWeights = GateWeights
                  { wL = [[0, 0]
                         ,[1, 0]
                         ,[0, 1]
                         ,[0, 0]
                         ,[0, 0]]
                  , wR = [[0, 0]
                         ,[0, 0]
                         ,[0, 0]
                         ,[1, 0]
                         ,[0, 1]]
                  , wO = [[1, -1]
                         ,[0, 0]
                         ,[0, 0]
                         ,[0, 0]
                         ,[0, 0]]
                  }

main :: IO ()
main = runExample
