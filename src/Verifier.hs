{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Verifier where

import Debug.Trace (trace)
import qualified Numeric.LinearAlgebra as HMatrix
import Numeric.LinearAlgebra.Data hiding ((<>))
import Primitives
import Sensitivity
import Sensitivity (SDouble)
import Test.QuickCheck (Arbitrary (arbitrary), quickCheck)

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

instance Arbitrary (SDouble Diff '[]) where
  arbitrary = D_UNSAFE <$> arbitrary @Double

-- Required for quickCheck
instance Show (SDouble Diff '[]) where
  show sdouble = "SDouble Diff '[] " <> show (unSDouble sdouble)

-- TODO at a later point create Arbitrary instance for: forall s. SDouble Diff s

prop_distance_solo :: SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> Bool
prop_distance_solo a1 a2 b1 b2 =
  let d1 = abs $ unSDouble a1 - unSDouble a2
      d2 = abs $ unSDouble b1 - unSDouble b2
      dout = abs $ unSDouble (f a1 b1) - unSDouble (f a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- So, how would we make sensitivity annotations on hmatrix functions?
-- Maybe I can start on something simple like proving Matrix SDouble * Constant has the same sensitivity
-- e.g. (matrix 3 [1..9] :: Matrix SDouble ) * matrix 1 [2]
-- where the first one is our sensitive doubles and the second is just a constant.

-- Going to start by making a Matrix with SDoubles in it. Going to keep it not polymorphic for now.
-- Assumption: Multiplying by a constant does not effect the sensitivity
type SDoubleMatrix s = HMatrix.Matrix (SDouble Diff s)

-- Assert that the above function's senstivity annotation is correct
prop_safe_multiply_constant matrix = undefined

-- Without Solo - L2 sensitivity of matrix addition
safe_add :: HMatrix.Matrix Double -> HMatrix.Matrix Double -> HMatrix.Matrix Double
safe_add m1 m2 = m1 + m2

-- TODO Syed: Create Gen for Matrix Double
prop_safe_add a1 a2 b1 b2 =
  let d1 = HMatrix.norm_2 (a1 - a2) -- L2 distance between first arguments
      d2 = HMatrix.norm_2 (b1 - b2) -- L2 distance between second arguments
      -- L2 distance between two outputs
      dout = HMatrix.norm_2 $ (safe_add a1 b1) - (safe_add a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- Intermediate representation of a sensative double matrix that our clients use
-- Use this data type when you need to do any hmatrix operations such as
-- addition, multiplication, etc
newtype SDInterMatrix (s :: SEnv) = SDInterMatrix [[SDouble Diff s]]

safe_add_solo :: SDInterMatrix s1 -> SDInterMatrix s2 -> SDInterMatrix (s1 +++ s2)
safe_add_solo sdim1 sdim2 =
  -- Convert intermediate sensative matrix to double matrix and add them
  let doubleMatrix = unsafeToDoubleMatrix sdim1 + unsafeToDoubleMatrix sdim2
      -- Convert from matrix back to a list and rewrap the sensitive double
      sdoubles = (fmap . fmap) D_UNSAFE $ HMatrix.toLists doubleMatrix
   in SDInterMatrix sdoubles

-- This is an internal helper function that exposes hmatrix operations that we wouldn't export
unsafeToDoubleMatrix :: SDInterMatrix s -> Matrix Double
unsafeToDoubleMatrix (SDInterMatrix a) = HMatrix.fromLists $ (fmap . fmap) unSDouble a
