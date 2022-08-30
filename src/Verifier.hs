{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Verifier where

import Control.Monad (replicateM)
import qualified Data.Matrix as Matrix
import Debug.Trace (trace)
import Sensitivity (CMetric (..), NMetric (Diff), SDouble (..), SEnv, type (+++))

-- TODO rename this module to represent user defined functions. Remove props.

-- This is a function from an external library for example hmatrix
unsafe_f :: Double -> Double -> Double
unsafe_f a b = a + b

-- Assert that |x1-x2| + |y1 - y2| <= |f(x1,x2) - f(x2, y2)|
prop_distance :: Double -> Double -> Double -> Double -> Bool
prop_distance x1 y1 x2 y2 =
  let d1 = abs (x1 - x2)
      d2 = abs (y1 - y2)
      dout = abs $ unsafe_f x1 y2 - unsafe_f x2 y2
   in dout <= d1 + d2

-- This is a "developer" who's reexposed it with sensitivity annotations. But is it right? We will test that.
f :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
f a b = D_UNSAFE $ unsafe_f (unSDouble a) (unSDouble b)

-- Example quasiquoter
-- [gen| f :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) |]

prop_distance_solo :: SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> Bool
prop_distance_solo a1 a2 b1 b2 =
  let d1 = abs $ unSDouble a1 - unSDouble a2
      d2 = abs $ unSDouble b1 - unSDouble b2
      dout = abs $ unSDouble (f a1 b1) - unSDouble (f a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- Without Solo - L2 sensitivity of matrix addition
safe_add :: Matrix.Matrix Double -> Matrix.Matrix Double -> Matrix.Matrix Double
safe_add m1 m2 = m1 + m2

prop_safe_add ::
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Bool
prop_safe_add a1 a2 b1 b2 =
  let d1 = norm_2 (a1 - a2) -- L2 distance between first arguments
      d2 = norm_2 (b1 - b2) -- LsuchThat2 distance between second arguments
      -- L2 distance between two outputs
      dout = norm_2 $ (safe_add a1 b1) - (safe_add a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- L2 norm of a Matrix
norm_2 :: Floating n => Matrix.Matrix n -> n
norm_2 m = sqrt $ foldr (\x acc -> acc + abs x ** 2) 0 m

-- TODO L1 https://programming-dp.com/notebooks/ch6.html#vector-valued-functions-and-their-sensitivities

-- Similar to SList we need a custom data type
newtype SMatrix (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SMatrix_UNSAFE {unSMatrix :: Matrix.Matrix (f s)}

-- A Matrix of SDoubles with L2 Metrix
type SDoubleMatrixL2 s = SMatrix L2 (SDouble Diff) s

-- With Solo - L2 sensitivity of matrix addition
safe_add_solo :: SDoubleMatrixL2 s1 -> SDoubleMatrixL2 s2 -> SDoubleMatrixL2 (s1 +++ s2)
safe_add_solo m1 m2 =
  SMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unSMatrix m1) + (unSDouble <$> unSMatrix m2)

toDoubleMatrix :: SMatrix m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix m = unSDouble <$> unSMatrix m

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
      dout = norm_2 $ toDoubleMatrix (safe_add_solo a1 b1) - toDoubleMatrix (safe_add_solo a2 b2)
   in dout <= d1 + d2 + 0.000000001

absdist :: Floating n => n -> n
absdist = undefined

l1dist :: Floating n => n -> n
l1dist = undefined

l2dist :: Floating n => n -> n
l2dist = undefined

diff :: Floating n => n -> n
diff = undefined
