{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module SensStaticHMatrix where

import GHC.TypeLits (Nat, SomeNat (SomeNat))
import GHC.TypeLits qualified as TL
import GHC.TypeNats (KnownNat, SNat)
import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra
    ( fromLists, Element, Transposable(tr), Extractor (Pos) )
import Sensitivity (CMetric (..), DPSDoubleMatrixL2, DPSMatrix (DPSMatrix_UNSAFE, unDPSMatrix), JoinSens, NMetric (Diff), SDouble (..), SDoubleMatrixL2, SEnv, SList, SMatrix (SMatrix_UNSAFE, unSMatrix), SPair (P_UNSAFE), ScaleSens, type (+++))
import Utils
import Test.QuickCheck
import Data.Proxy
import Prelude hiding ((<>))
import Control.Monad (replicateM)
import Data.Sequence (chunksOf)
import GHC.TypeNats ( withSomeSNat )
import GHC.Num.Natural (Natural)
import Data.Reflection
import Primitives (plus_cong)
import Test.QuickCheck.Test (test)

-- | SensStaticHMatrix is a wrapper around Numeric.LinearAlgebra.Matrix.Static
-- A sensitive wrapper of HMatrix's depedendently typed Matrix
-- For the non dependently typed Matrix see SensHMatrix
-- HMatrix doesn't have a functor instance so we need to instead reference the inner type directly
-- e.g. instead of using Matrix SDouble Diff senv we create a newtype wrapper around Matrix Double
newtype SensStaticHMatrix (x :: Nat) (y :: Nat) (m :: CMetric) (n :: NMetric) (s :: SEnv) =
  SensStaticHMatrixUNSAFE {unSensStaticHMatrix :: L x y}

instance (forall senv. KnownNat x, KnownNat y) => Arbitrary (SensStaticHMatrix x y cmetric nmetric s1) where
   arbitrary = do
    -- Note that I'm not generating matrices of aribitary row and column size unlike SMatrix. This becomes useful in generating matrices that require rows and cols to match.
    let row = TL.natVal @x Proxy
        col = TL.natVal @y Proxy
    -- Generate a list elements of size row * col
    elems <- replicateM (fromInteger (row * col)) arbitrary
    pure $ SensStaticHMatrixUNSAFE $ matrix elems

instance (KnownNat x, KnownNat y) => Show (SensStaticHMatrix x y m n s) where
  show smatrix = show $ unSensStaticHMatrix smatrix

-- Generate a matrix given a row and column size
-- genMatrix :: Arbitrary a => Int -> Int -> Gen (Matrix.Matrix a)
-- genMatrix row col = do
--   -- Generate a list of arbitrary elements of size row * col
--   elems <- replicateM (row * col) arbitrary
--   -- Convert to Matrix
--   pure $ Matrix.fromList row col elems

plus :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s2 -> SensStaticHMatrix x y cmetric nmetric (s1 +++ s2)
plus m1 m2 =
  SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 + unSensStaticHMatrix m2

subtract :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s2 -> SensStaticHMatrix x y cmetric nmetric (s1 +++ s2)
subtract m1 m2 =
  SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 - unSensStaticHMatrix m2

-- TODO implement scale by typelevel Nat x
-- ScaleSens senv
-- Why did I suggest this?

-- Hmm is mult +++? Also this isn't the correct sensitivity annotation
mult :: (KnownNat x, KnownNat k, KnownNat y) => SensStaticHMatrix x k cmetric nmetric s1 -> SensStaticHMatrix k y cmetric nmetric s2 -> SensStaticHMatrix x y cmetric nmetric (s1 +++ s2)
mult m1 m2 = SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 <> unSensStaticHMatrix m2

transpose :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix y x cmetric nmetric s1
transpose m1 = SensStaticHMatrixUNSAFE $ tr $ unSensStaticHMatrix m1

gen ::
  forall x y c n s1.
  (KnownNat x, KnownNat y) =>
  Gen (SensStaticHMatrix x y c n s1)
gen = do
  elems1 <- replicateM (fromInteger (reflect (Proxy @x) * reflect (Proxy @y))) (arbitrary @Double)
  Debug.traceShowM elems1
  pure (SensStaticHMatrixUNSAFE $ matrix elems1)

arbitraryKnownNat :: Gen SomeNat
arbitraryKnownNat = do
  Debug.traceM "arbitraryKnownNat1"
  (Positive x') <- arbitrary @(Positive Integer)
  reifyNat x' $ \(Proxy @x) -> 
    pure (SomeNat @x Proxy)

-- Generate two of the same sized matrix
genTwo ::
  (forall n m.
   KnownNat n =>
   KnownNat m =>
   SensStaticHMatrix n m L2 Diff s ->
   SensStaticHMatrix n m L2 Diff s ->
   SensStaticHMatrix n m L2 Diff s ->
   SensStaticHMatrix n m L2 Diff s ->
   Gen r) ->
  Gen r
genTwo cond = do
  SomeNat @x _ <- arbitraryKnownNat
  SomeNat @y _ <- arbitraryKnownNat
  m1 <- gen @x @y @L2 @Diff
  m2 <- gen @x @y @L2 @Diff
  m3 <- gen @x @y @L2 @Diff
  m4 <- gen @x @y @L2 @Diff
  cond m1 m2 m3 m4

-- Generate two matrices for multiplication
genTwoMult ::
  (forall n m k.
   KnownNat n =>
   KnownNat m =>
   KnownNat k =>
   SensStaticHMatrix n m L2 Diff s ->
   SensStaticHMatrix m k L2 Diff s ->
   Gen r) ->
  Gen r
genTwoMult cond = do
  SomeNat @x _ <- arbitraryKnownNat
  SomeNat @y _ <- arbitraryKnownNat
  SomeNat @z _ <- arbitraryKnownNat
  m1 <- gen @x @y @L2 @Diff
  m2 <- gen @y @z @L2 @Diff
  cond m1 m2

