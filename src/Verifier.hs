{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Verifier where

import Primitives
import Sensitivity
import Sensitivity (SDouble)
import Test.QuickCheck (Arbitrary (arbitrary), quickCheck)

-- This is a function from an external library for example hmatrix
unsafe_f :: Double -> Double -> Double
unsafe_f a b = a + b

-- Assert that |a1-a2| + |b1 - b2| <= |f(a1,b2) - f(a2, b2)|
prop_distance :: Double -> Double -> Double -> Double -> Bool
prop_distance a1 a2 b1 b2 =
  let d1 = abs (a1 - a2)
      d2 = abs (b1 - b2)
      dout = abs $ (unsafe_f a1 b1) - (unsafe_f a2 b2)
   in d1 + d2 <= dout

-- This is a "developer" who's reexposed it with sensitivity annotations. But is it right? We will test that.
f :: forall s1 s2. SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
f a b = D_UNSAFE $ unSDouble a * unSDouble b

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
   in d1 + d2 <= dout
