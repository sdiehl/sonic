{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Test.Reference where

import Protolude hiding (Semiring)
import Bulletproofs.ArithmeticCircuit (ArithCircuit(..), Assignment(..), GateWeights(..))
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Field.Galois (rnd)
import Data.Pairing.BLS12381 (Fr)
import Data.Poly.Sparse.Laurent (VLaurent)
import Data.Semiring (Semiring)
import qualified GHC.Exts

import Test.QuickCheck

getZeroCoeff :: (Eq f, Semiring f) => VLaurent f -> Maybe f
getZeroCoeff p = case filter ((==) 0 . fst) . GHC.Exts.toList $ p of
  [] -> Nothing
  [(_, z)] -> Just z
  _ -> panic "Impossibly many zero coefficients"

-------------
-- Examples
-------------

-- Example 1
--
-- bL0     bR0    bL1      10
--  |       |      |       |
--  |--[+]--|      |--[+]--|
--      |              |
--      | bO0      bO1 |
--      |  =        =  |
--      |  aL      aR  |
--      |-----[x]------|
--             |
--             | aO
--             |
arithCircuitExample1 :: Fr -> Fr -> (ArithCircuit Fr, Assignment Fr)
arithCircuitExample1 x z =
  let wL = [[1], [0]]
      wR = [[0], [1]]
      wO = [[0], [0]]
      cs = [7 + 3, 2 + 10]
      aL = [10]
      aR = [12]
      aO = zipWith (*) aL aR
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      circuit = ArithCircuit gateWeights [] cs
  in (circuit, assignment)

-- Example 2
-- 5 linear constraint (q = 5):
-- aO[0] = aO[1]
-- aL[0] = V[0] - z
-- aL[1] = V[2] - z
-- aR[0] = V[1] - z
-- aR[1] = V[3] - z
--
-- 2 multiplication constraint (implicit) (n = 2):
-- aL[0] * aR[0] = aO[0]
-- aL[1] * aR[1] = aO[1]
--
-- 4 input values (m = 4)
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
      aO = zipWith (*) aL aR
      gateWeights = GateWeights wL wR wO
      assignment = Assignment aL aR aO
      circuit = ArithCircuit gateWeights witness cs
  in (circuit, assignment)

-- As stated in the paper:
-- "...in our polynomial constraint system 3n < d
-- (otherwise we cannot commit to t(X,Y)),
-- thus r(X,Y) has no (−d + n) term."
--
-- Moreover, 'Sonic.Protocol.prove' requires d >= 7n.
-- Further, the existing implementation of
-- 'Sonic.CommitmentScheme.{commit,open}Poly'
-- for some reason requires d >= 12 for n = 1 and d >= 16 for n = 2.
randomD :: MonadRandom m => Int -> m Int
randomD 1 = getRandomR (12, 100)
randomD 2 = getRandomR (16, 200)
randomD n = getRandomR (7 * n, 100 * n)

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

-------------------
-- Arbitrary
-------------------

rndCircuit :: Gen (ArithCircuit Fr, Assignment Fr)
rndCircuit = do
  n <- choose (1, 20)
  m <- choose (1, n)
  assignment <- arithAssignmentGen n
  circuit <- arithCircuitGen assignment m
  pure (circuit, assignment)

arithCircuitGen :: Assignment Fr -> Int -> Gen (ArithCircuit Fr)
arithCircuitGen assignment@Assignment{aL, aR, aO} m = do
  weights@GateWeights{..} <- gateWeightsGen m n
  let gateWeights = GateWeights wL wR wO

  let cs = traverseDot aL wL ^+^ traverseDot aR wR ^+^ traverseDot aO wO
  pure $ ArithCircuit gateWeights witness cs
    where
      gateWeightsGen :: Int -> Int -> Gen (GateWeights Fr)
      gateWeightsGen l n = do
        let genVec = (\i ->
                         insertAt i (oneVector n) (replicate (l - 1) (zeroVector n))
                     ) <$> choose (0, l)
        wL <- genVec
        wR <- genVec
        wO <- genVec
        pure $ GateWeights wL wR wO

      zeroVector x = replicate x 0
      oneVector x = replicate x 1

      insertAt :: Int -> a -> [a] -> [a]
      insertAt z y xs = let (as, bs) = splitAt z xs in as ++ (y : bs)

      n = length aL

      traverseDot :: [Fr] -> [[Fr]] -> [Fr]
      traverseDot v m = sum . zipWith (*) v <$> m

      (^+^) = zipWith (+)

arithAssignmentGen :: Int -> Gen (Assignment Fr)
arithAssignmentGen n = do
    aL <- vectorOf n arbitrary
    aR <- vectorOf n arbitrary
    let aO = zipWith (*) aL aR
    pure $ Assignment aL aR aO
