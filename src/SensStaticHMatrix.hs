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

-- | SensStaticHMatrix is a wrapper around Numeric.LinearAlgebra.Matrix.Static
-- A sensitive wrapper of HMatrix's depedendently typed Matrix
-- For the non dependently typed Matrix see SensHMatrix
-- HMatrix doesn't have a functor instance so we need to instead reference the inner type directly
-- e.g. instead of using Matrix SDouble Diff senv we create a newtype wrapper around Matrix Double
-- TODO make this less or more polymorphic. Static matrix only works on Naturals anyway.
newtype SensStaticHMatrix (x :: Nat) (y :: Nat) (m :: CMetric) (type' :: SEnv -> *) (s :: SEnv) = 
  SensStaticHMatrixUNSAFE {unSensStaticHMatrix :: L x y}


instance (forall senv. Arbitrary (type' senv), KnownNat x, KnownNat y) => Arbitrary (SensStaticHMatrix x y cmetric type' s1) where
   arbitrary = do
    -- Note that I'm not generating matrices of aribitary row and column size unlike SMatrix. This becomes useful in generating matrices that require rows and cols to match.
    let row = TL.natVal @x Proxy
        col = TL.natVal @y Proxy
    -- Generate a list elements of size row * col
    elems <- replicateM (fromInteger (row * col)) arbitrary
    pure $ SensStaticHMatrixUNSAFE $ matrix elems

instance (Show (type' s), KnownNat x, KnownNat y) => Show (SensStaticHMatrix x y m type' s) where
  show smatrix = show $ unSensStaticHMatrix smatrix

plus :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric (SDouble metric) s1 -> SensStaticHMatrix x y cmetric (SDouble metric) s2 -> SensStaticHMatrix x y cmetric (SDouble metric) (s1 +++ s2)
plus m1 m2 =
  SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 + unSensStaticHMatrix m2

subtract :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric (SDouble metric) s1 -> SensStaticHMatrix x y cmetric (SDouble metric) s2 -> SensStaticHMatrix x y cmetric (SDouble metric) (s1 +++ s2)
subtract m1 m2 =
  SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 - unSensStaticHMatrix m2

-- TODO implement scale by typelevel Nat x
-- ScaleSens  senv

-- Hmm is mult +++?
mult :: (KnownNat x, KnownNat k, KnownNat y) => SensStaticHMatrix x k cmetric (SDouble metric) s1 -> SensStaticHMatrix k y cmetric (SDouble metric) s2 -> SensStaticHMatrix x y cmetric (SDouble metric) (s1 +++ s2)
mult m1 m2 = SensStaticHMatrixUNSAFE $ unSensStaticHMatrix m1 <> unSensStaticHMatrix m2

-- Not sure what this is
transpose :: (KnownNat x, KnownNat y) => SensStaticHMatrix x y cmetric (SDouble metric) s1 -> SensStaticHMatrix y x cmetric (SDouble metric) s1
transpose m1 = SensStaticHMatrixUNSAFE $ tr $ unSensStaticHMatrix m1
