{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Verifier where

import Control.Monad (replicateM)
import qualified Data.Matrix as Matrix
import Debug.Trace (trace)
import Sensitivity (NMetric (Diff), SDouble (..), type (+++))

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
f :: forall s1 s2. SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
f a b = D_UNSAFE $ unSDouble a + unSDouble b

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

-- With Solo - L2 sensitivity of matrix addition
safe_add_solo :: Matrix.Matrix (SDouble Diff s1) -> Matrix.Matrix (SDouble Diff s2) -> Matrix.Matrix (SDouble Diff (s1 +++ s2))
safe_add_solo m1 m2 = D_UNSAFE <$> (unSDouble <$> m1) + (unSDouble <$> m2)

prop_safe_add_solo ::
  Matrix.Matrix (SDouble 'Diff '[]) ->
  Matrix.Matrix (SDouble 'Diff '[]) ->
  Matrix.Matrix (SDouble 'Diff '[]) ->
  Matrix.Matrix (SDouble 'Diff '[]) ->
  Bool
prop_safe_add_solo a1 a2 b1 b2 =
  let toDoubleMatrix m = unSDouble <$> m
      d1 = norm_2 $ toDoubleMatrix a1 - toDoubleMatrix a2 -- L2 distance between first arguments
      d2 = norm_2 $ toDoubleMatrix b1 - toDoubleMatrix b2
      -- L2 distance between two outputs
      dout = norm_2 $ toDoubleMatrix (safe_add_solo a1 b1) - toDoubleMatrix (safe_add_solo a2 b2)
   in dout <= d1 + d2 + 0.000000001
