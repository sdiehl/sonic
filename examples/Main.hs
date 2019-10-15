{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Protolude
import Control.Monad.Random (getRandomR)
import Bulletproofs.ArithmeticCircuit
import Data.Pairing.BLS12381 (Fr)
import Data.Field.Galois (rnd)

import Sonic.SRS as SRS
import Sonic.Protocol

sonicProtocol :: ArithCircuit Fr -> Assignment Fr -> Fr -> IO Bool
sonicProtocol circuit assignment x = do
  -- Setup for an SRS
  srs <- SRS.new <$> randomD n <*> pure x <*> rnd
  -- Prover
  (proof, rndOracle@RndOracle{..}) <- prove srs assignment circuit
  -- Verifier
  pure $ verify srs circuit proof rndOracleY rndOracleZ rndOracleYZs
  where
    -- n: Number of multiplication constraints
    n = length $ aL assignment
    randomD n = getRandomR (7 * n, 100 * n)

-- 5 linear constraints (q = 5):
-- aO[0] = aO[1]
-- aL[0] = V[0] - z
-- aL[1] = V[2] - z
-- aR[0] = V[1] - z
-- aR[1] = V[3] - z
--
-- 2 multiplication constraints (implicit) (n = 2):
-- aL[0] * aR[0] = aO[0]
-- aL[1] * aR[1] = aO[1]
--
-- 4 input values (m = 4)
arithCircuitExample :: Fr -> Fr -> (ArithCircuit Fr, Assignment Fr)
arithCircuitExample x z =
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

      cs = [0, 4-z, 9-z, 9-z, 4-z]
      aL = [4 - z, 9 - z]
      aR = [9 - z, 4 - z]
      aO = zipWith (*) aL aR
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      circuit = ArithCircuit gateWeights witness cs
  in (circuit, assignment)

runExample :: IO ()
runExample = do
  pX <- rnd
  pZ <- rnd
  let (arithCircuit, assignment@Assignment{..}) = arithCircuitExample pX pZ
  success <- sonicProtocol arithCircuit assignment pX
  putText $ "Success: " <> show success

main :: IO ()
main = runExample
