{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}


module AnnotatedExternalLibrary where

import Control.Monad (replicateM)
import qualified Data.Matrix as Matrix
import Debug.Trace (trace)
import Sensitivity (CMetric (..), NMetric (Diff), SDouble (..), SEnv, type (+++), SDoubleMatrixL2, SMatrix (unSMatrix, SMatrix_UNSAFE))
import DistanceFunctions
import Utils

-- | This Module simulates a developer re-exposing "unsafe" external libraries as solo annotated functions
-- Also includes examples of manually generated props

-- This is a function from an external library for example hmatrix
unsafe_function :: Double -> Double -> Double
unsafe_function a b = a + b

-- Assert that |x1-x2| + |y1 - y2| <= |f(x1,x2) - f(x2, y2)|
unsafe_unsafe_external_function_prop :: Double -> Double -> Double -> Double -> Bool
unsafe_unsafe_external_function_prop x1 y1 x2 y2 =
  let d1 = abs (x1 - x2)
      d2 = abs (y1 - y2)
      dout = abs $ unsafe_function x1 y2 - unsafe_function x2 y2
   in dout <= d1 + d2

-- This is a "developer" who's reexposed it with sensitivity annotations. But is it right? We will test that.
external_function :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
external_function a b = D_UNSAFE $ unsafe_function (unSDouble a) (unSDouble b)

external_function_prop :: SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> Bool
external_function_prop a1 a2 b1 b2 =
  let d1 = abs $ unSDouble a1 - unSDouble a2
      d2 = abs $ unSDouble b1 - unSDouble b2
      dout = abs $ unSDouble (external_function a1 b1) - unSDouble (external_function a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- Without Solo - L2 sensitivity of matrix addition
unsafe_add :: Matrix.Matrix Double -> Matrix.Matrix Double -> Matrix.Matrix Double
unsafe_add m1 m2 = m1 + m2

prop_unsafe_add ::
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Bool
prop_unsafe_add a1 a2 b1 b2 =
  let d1 = norm_2 (a1 - a2) -- L2 distance between first arguments
      d2 = norm_2 (b1 - b2) -- L2 distance between second arguments
      -- L2 distance between two outputs
      dout = norm_2 $ unsafe_add a1 b1 - unsafe_add a2 b2
   in dout <= d1 + d2 + 0.000000001


-- With Solo - L2 sensitivity of matrix addition
add_matrix_solo :: SDoubleMatrixL2 s1 -> SDoubleMatrixL2 s2 -> SDoubleMatrixL2 (s1 +++ s2)
add_matrix_solo m1 m2 =
  SMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unSMatrix m1) + (unSDouble <$> unSMatrix m2)


prop_safe_add_solo ::
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  Bool
prop_safe_add_solo a1 a2 b1 b2 =
  let d1 = norm_2 $ toDoubleMatrix a1 - toDoubleMatrix a2 -- L2 distance between first arguments
      d2 = norm_2 $ toDoubleMatrix b1 - toDoubleMatrix b2
      -- L2 distance between two outputs
      dout = norm_2 $ toDoubleMatrix (add_matrix_solo a1 b1) - toDoubleMatrix (add_matrix_solo a2 b2)
   in dout <= d1 + d2 + 0.000000001
