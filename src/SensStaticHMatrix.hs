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

import GHC.TypeLits (Nat)
import GHC.TypeLits qualified as TL
import GHC.TypeNats (KnownNat)
import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra
    ( fromLists, Element, Transposable(tr) )
import Sensitivity (CMetric (..), DPSDoubleMatrixL2, DPSMatrix (DPSMatrix_UNSAFE, unDPSMatrix), JoinSens, NMetric (Diff), SDouble (..), SDoubleMatrixL2, SEnv, SList, SMatrix (SMatrix_UNSAFE, unSMatrix), SPair (P_UNSAFE), ScaleSens, type (+++))
import Utils
import Test.QuickCheck
import Data.Proxy
import Prelude hiding ((<>))
import Control.Monad (replicateM)
import Data.Sequence (chunksOf)
import GHC.TypeNats ( withSomeSNat )
import GHC.Num.Natural (Natural)
import Primitives (plus_cong)

-- | SensStaticHMatrix is a wrapper around Numeric.LinearAlgebra.Matrix.Static
-- A sensitive wrapper of HMatrix's depedendently typed Matrix
-- For the non dependently typed Matrix see SensHMatrix
-- HMatrix doesn't have a functor instance so we need to instead reference the inner type directly
-- e.g. instead of using Matrix SDouble Diff senv we create a newtype wrapper around Matrix Double
-- TODO make this less or more polymorphic
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
-- ScaleSens  senv

-- Hmm is mult +++?
mult :: (KnownNat x, KnownNat k, KnownNat y) => SensStaticHMatrix x k cmetric nmetric s1 -> SensStaticHMatrix k y cmetric nmetric s2 -> SensStaticHMatrix x y cmetric nmetric (s1 +++ s2)
mult m1 m2 = SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 <> unSensStaticHMatrix m2

-- Not sure what this is
transpose :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix y x cmetric nmetric s1
transpose m1 = SensStaticHMatrixUNSAFE $ tr $ unSensStaticHMatrix m1


-- plusWithSomeNat :: SensStaticHMatrix x y cmetric (SDouble metric) s1 -> SensStaticHMatrix   cmetric      (SDouble metric)      s2 -> SensStaticHMatrix      ghc-prim-0.10.0:GHC.Types.Any      ghc-prim-0.10.0:GHC.Types.Any      cmetric      (SDouble metric)      (s1 +++ s2)
-- hmm this doesn't wokr but also the type signature doesn't make sense
-- plusWithSomeNat = (withKnownNat2 plus) 5 8

-- https://stackoverflow.com/questions/39755675/quickchecking-a-property-about-length-indexed-lists
-- Even better https://discourse.haskell.org/t/how-to-create-arbitrary-instance-for-dependent-types/6990/39
data BoxSHMatrix cmetric nmetric s where
  Box :: SensStaticHMatrix x y cmetric nmetric s -> BoxSHMatrix cmetric nmetric s

instance Show (BoxSHMatrix cmetric nmetric s) where
  -- Kind of hard to make this work since I need to know the size of the matrix
  --show (Box m) = show $ unSensStaticHMatrix @1 @1 @cmetric @nmetric m
  show (Box m) = "BoxSHMatrix"

-- Generate a matrix given a row and column size
genMatrix :: forall cmetric nmetric s. (Arbitrary (SDouble nmetric s)) => Int -> Int -> Gen (BoxSHMatrix cmetric nmetric s)
genMatrix row col = do
  -- Generate a list of arbitrary elements of size row * col
  elems <- replicateM ( row * col) (arbitrary @(SDouble nmetric s))
  -- in the example they construct this from scratch 
  -- We need to do the same otherwise we get an error
  -- This does not work for example
  -- pure $ Box $ SensStaticHMatrixUNSAFE $ matrix elems
  pure $ Box $ SensStaticHMatrixUNSAFE undefined --_ $ chunksOf col elems

-- test = do
--   (x, y) <- generate $ (,) <$> choose (1, 10) <*> choose (1, 10)
--   (Box m1) <- generate $ genMatrix @L2 @Diff x y
--   (Box m2) <- generate $ genMatrix @L2 @Diff x y
--   pure $ plus m1 m2

-- prop_plus f = forAll (genMatrix @L2 @Diff 3 3) $ \(Box m1) ->
--               forAll (genMatrix @L2 @Diff 3 3) $ \(Box m2) ->
--               True
--               -- TODO
--               -- f m1 m2


-- prop_plus2 :: (SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s2 -> Bool) -> Bool
-- prop_plus2 f = do
--   (Box m1) <- genMatrix @L2 @Diff 3 3
--   (Box m2) <- genMatrix @L2 @Diff 3 3
--   pure $ f m1 m2

-- Ok well that failed so what if we have really specialized existential
data BoxSHMatrix2 (cmetric :: CMetric) (nmetric :: NMetric) s1 s2 where
  Box2 :: SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s2 -> BoxSHMatrix2 cmetric nmeric s1 s2

instance Show (BoxSHMatrix2 cmetric nmetric s1 s2) where
  -- Kind of hard to make this work since I need to know the size of the matrix
  --show (Box m) = show $ unSensStaticHMatrix @1 @1 @cmetric @nmetric m
  show (Box2 m1 m2) = "BoxSHMatrix"

-- Generate a matrix given a row and column size
genMatrix2 :: forall cmetric nmetric s1 s2. (Arbitrary (SDouble nmetric s1), Arbitrary (SDouble nmetric s2)) => Int -> Int -> Gen (BoxSHMatrix2 cmetric nmetric s1 s2)
genMatrix2 row col = do
  -- Generate a list of arbitrary elements of size row * col
  elems <- replicateM ( row * col) (arbitrary @(SDouble nmetric s1))
  -- in the example they construct this from scratch 
  -- We need to do the same otherwise we get an error
  -- This does not work for example
  -- pure $ Box $ SensStaticHMatrixUNSAFE $ matrix elems
  pure undefined

-- test = do
--   (x, y) <- generate $ (,) <$> choose (1, 10) <*> choose (1, 10)
--   (Box2 m1 m2) <- generate $ genMatrix2 @L2 @Diff x y
--   pure $ plus m1 m2

fakePlusProp :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s1 -> SensStaticHMatrix x y cmetric nmetric s2 -> SensStaticHMatrix x y cmetric nmetric s2 -> Bool
fakePlusProp _ _ _ _ = True


-- Doesn't work either
-- testStaticPlus = do
  -- (x, y) <- generate $ (,) <$> choose (1, 10) <*> choose (1, 10)
  -- box1 <- generate $ SensStaticHMatrix.genMatrix @L2 @Diff x y
  -- box2 <- generate $ SensStaticHMatrix.genMatrix @L2 @Diff x y
  -- case (box1, box2) of (SensStaticHMatrix.Box m1, SensStaticHMatrix.Box m2) -> print (fakePlusProp m1 m1 m2 m2)
