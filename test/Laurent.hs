{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Laurent where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import Math.Polynomial.Laurent

import Sonic.Curve (Fr)
import Sonic.Utils

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Laurent a) where
    arbitrary = newLaurent <$> arbitrary <*> arbitrary

test_laurentTests :: TestTree
test_laurentTests
  = testGroup "Laurent polynomials"
      [ testGroup "Multivariate polynomials"
        [ testProperty "sane" $ \p x y ->
            (x /= 0 && y /= 0) ==>
             evalLaurent @Fr (evalOnY y p) x == evalLaurent (evalOnX x p) y
        , testProperty "lift poly X" $ \p x y ->
            (x /= 0 && y /= 0) ==>
              evalLaurent @Fr (evalOnY y (convertToTwoVariateX p)) x == evalLaurent p x
        , testProperty "lift poly Y" $ \p x y ->
            (x /= 0 && y /= 0) ==>
              evalLaurent @Fr (evalOnX x (convertToTwoVariateY p)) y == evalLaurent p y
        ]
      ]

