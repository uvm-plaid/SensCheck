{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{- | Contains quickcheck tests for Verifier
 TODO this should be in the test/ directory but currently having issues with HLS
-}
module VerifierQuickCheck where

import Control.Monad (replicateM)
import qualified Data.Matrix as Matrix
import Sensitivity (NMetric (Diff), SDouble (..))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Positive (Positive), forAll, quickCheck, suchThat, withMaxSuccess)
import Verifier (prop_distance, prop_distance_solo, prop_safe_add, prop_safe_add_solo)

test_distance :: IO ()
test_distance = quickCheck (withMaxSuccess 1000 prop_distance)

test_distance_solo :: IO ()
test_distance_solo = quickCheck (withMaxSuccess 1000 prop_distance_solo)

instance Arbitrary (SDouble Diff s) where
  arbitrary = D_UNSAFE <$> arbitrary @Double

-- Required for quickCheck
instance Show (SDouble Diff '[]) where
  show sdouble = "SDouble Diff '[] " <> show (unSDouble sdouble)

-- TODO at a later point create Arbitrary instance for: forall s. SDouble Diff s

-- Generate a matrix given a row and column size
genMatrix :: Arbitrary a => Int -> Int -> Gen (Matrix.Matrix a)
genMatrix row col = do
  -- Generate a list of arbitrary elements of size row * col
  elems <- replicateM (row * col) arbitrary
  -- Convert to Matrix
  pure $ Matrix.fromList row col elems

-- This generates matrixes of the same dimensions
-- The number of matrixes generated is provided by numMatrixes
-- This helps prevent calling functions passing in matrixes of various sizes
-- and failing due to invalid operations
genMatrixes :: Arbitrary a => Int -> Gen [Matrix.Matrix a]
genMatrixes numMatrixes = do
  -- Generate an arbitrary number of rows and columns greater than 0
  (Positive row) <- arbitrary @(Positive Int)
  (Positive col) <- arbitrary @(Positive Int)
  -- Generate matrixes of the same row and col size
  replicateM numMatrixes $ genMatrix row col

test_add :: IO ()
test_add =
  quickCheck $
    withMaxSuccess 1000 $
      forAll -- Generate 4 matrixes of the same size
        (genMatrixes 4)
        (\[m1, m2, m3, m4] -> prop_safe_add m1 m2 m3 m4)

test_add_solo :: IO ()
test_add_solo =
  quickCheck $
    withMaxSuccess 1000 $
      forAll -- Generate 4 matrixes of the same size
        (genMatrixes 4)
        (\[m1, m2, m3, m4] -> prop_safe_add_solo m1 m2 m3 m4)
