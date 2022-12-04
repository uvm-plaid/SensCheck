{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Contains quickcheck tests for Verifier
 TODO this should be in the test/ directory but currently having issues with HLS
-}
module MatrixCorrectedSpec where

import Control.Monad (replicateM)
import qualified Data.Matrix as Matrix
import Sensitivity (NMetric (Diff), SDouble (..), SDoubleMatrixL2, SMatrix (SMatrix_UNSAFE))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Positive (Positive), forAll, quickCheck, suchThat, withMaxSuccess)
import AnnotatedExternalLibrary
import TH (genProp)
import DistanceFunctions
import Utils

-----------------------------------------------------------------------------------------------------------------
-- Below illustrates manual but correct ways to write quickcheck tests where inputs are dependent on each other
-- This is not possible to generate automatically currently. We could use dependent types.

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

genSDL2Matrixes :: Int -> Gen [SDoubleMatrixL2 s]
genSDL2Matrixes numMatrixes = (fmap . fmap) SMatrix_UNSAFE (genMatrixes numMatrixes)

-- Examples of proper quickcheck tests
-- The automatically generated ones 
-- test_add :: IO ()
-- test_add =
--   quickCheck $
--     withMaxSuccess 1000 $
--       forAll -- Generate 4 matrixes of the same size
--         (genMatrixes 4)
--         (\[m1, m2, m3, m4] -> prop_safe_add m1 m2 m3 m4)

-- prop_safe_add :: Matrix.Matrix a0 -> Matrix.Matrix a0 -> Matrix.Matrix a0 -> Matrix.Matrix a0 -> prop1
-- prop_safe_add = undefined

-- Automatically generated prop 
$(pure <$> genProp 'add_matrix_solo)

-- Corrected test that still uses automatically generated props
-- Note: This can be automatically generated with dependently typed matrixes
test_add_matrix_solo_corrected :: IO ()
test_add_matrix_solo_corrected =
  quickCheck $
    withMaxSuccess 1000 $
      forAll -- Generate 4 matrixes of the same size
        (genSDL2Matrixes 4)
        (\[m1, m2, m3, m4] -> add_matrix_solo_prop m1 m2 m3 m4)
