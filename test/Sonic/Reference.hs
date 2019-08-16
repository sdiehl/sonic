module Sonic.Reference where

import Protolude
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent
import Sonic.Utils

data Coeffs f = Coeffs
  { negCoeffs :: [f]
  , zeroCoeff :: Maybe f
  , posCoeffs :: [f]
  }

getCoeffs :: Laurent f -> Coeffs f
getCoeffs poly
  = if expL < 0
    then Coeffs
         (take (abs expL) coeffsL)
         (head $ drop (abs expL) coeffsL)
         (drop ((abs expL) + 1) coeffsL)
    else Coeffs [] (head coeffsL) (drop 1 coeffsL)
  where
    expL = expLaurent poly
    coeffsL = coeffsLaurent poly

getZeroCoeff :: Laurent f -> Maybe f
getZeroCoeff = zeroCoeff. getCoeffs

-------------
-- Examples
-------------

data ACExample f = ACExample
  { aceAssignment :: Assignment f
  , aceCircuit :: ArithCircuit f
  }

arithCircuitExample1 :: Num f => f -> f -> ACExample f
arithCircuitExample1 x z =
  let wL = [[1], [0]]
      wR = [[0], [1]]
      wO = [[0], [0]]
      cs = [7 + 3, 2 + 10]
      aL = [10]
      aR = [12]
      aO = aL `hadamardp` aR
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      arithCircuit = ArithCircuit gateWeights [] cs
  in ACExample assignment arithCircuit

arithCircuitExample2 :: Num f => f -> f -> ACExample f
arithCircuitExample2 x z =
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
      aO = aL `hadamardp` aR
      vs = [4, 9, 9, 4]
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      circuit = ArithCircuit gateWeights witness cs
  in ACExample assignment circuit


arithCircuitExample3 :: Num f => f -> f -> ACExample f
arithCircuitExample3 x z =
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
      wV = witness
      cs = [0, 4-z, 9-z, 9-z, 4-z]
      aL = [4 - z, 9 - z]
      aR = [9 - z, 4 - z]
      aO = aL `hadamardp` aR
      vs = [4, 9, 9, 4]
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      circuit = ArithCircuit gateWeights witness cs
  in ACExample assignment circuit
