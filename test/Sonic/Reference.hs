module Sonic.Reference where

import Protolude
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent
import Control.Monad.Random (MonadRandom, getRandomR)
import GaloisField(GaloisField(rnd))

import Sonic.Utils
import Sonic.Curve (Fr)

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

arithCircuitExample1 :: Fr -> Fr -> (ArithCircuit Fr, Assignment Fr)
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
      circuit = ArithCircuit gateWeights [] cs
  in (circuit, assignment)

arithCircuitExample2 :: Fr -> Fr -> (ArithCircuit Fr, Assignment Fr)
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
  in (circuit, assignment)

-- "...in our polynomial constraint system 3n < d
-- (otherwisewe cannot commit to t(X,Y)),
-- thus r(X,Y) has no (−d + n) term."
-- WARNING: Our constraint for the 'D' value used in the setup
-- needs to be greater than 6 times the number of constraints 'n'
randomD :: MonadRandom m => Int -> m Int
randomD n = getRandomR (7 * n, 50 * n)

data RandomParams = RandomParams
  { pX :: Fr
  , pY :: Fr
  , pZ :: Fr
  , pAlpha :: Fr
  }

randomParams :: MonadRandom m => m RandomParams
randomParams = do
  x <- rnd
  y <- rnd
  z <- rnd
  alpha <- rnd
  pure $ RandomParams x y z alpha
