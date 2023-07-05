{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module SHMatrix where

import Numeric.LinearAlgebra
import Distance
import Sensitivity (CMetric (..), DPSDoubleMatrixL2, DPSMatrix (DPSMatrix_UNSAFE, unDPSMatrix), NMetric (Diff), SDouble (..), SDoubleMatrixL2, SEnv, SMatrix (SMatrix_UNSAFE, unSMatrix), SPair (P_UNSAFE), type (+++), SList, JoinSens, ScaleSens)
import GHC.TypeLits (Nat)
import GHC.TypeLits qualified as TL
import GHC.TypeNats (KnownNat)
import Utils
import Prelude

-- HMatrix doesn't have a functor instance so we need to instead reference the inner type directly 
newtype SHMatrix (x :: Nat) (y :: Nat) (m :: CMetric) (f :: SEnv -> *) innerType (s :: SEnv) = SHMatrix_UNSAFE {unSHMatrix :: Matrix innerType}

-- TODO 
-- instance (forall senv. Arbitrary (innerType senv), KnownNat x, KnownNat y) => Arbitrary (DPSMatrix x y cmetric innerType s1) where
--   arbitrary = do
--     -- Note that I'm not generating matrices of aribitary row and column size unlike SMatrix. This becomes useful in generating matrices that require rows and cols to match.
--     let row = TL.natVal @x Proxy
--         col = TL.natVal @y Proxy
--     -- Generate a list elements of size row * col
--     elems <- replicateM (fromInteger (row * col)) arbitrary
--     -- Convert to Matrix
--     pure $ DPSMatrix_UNSAFE $ Matrix.fromList (fromInteger row) (fromInteger col) elems

-- instance (Show (f s)) => Show (DPSMatrix x y m f s) where
--   show smatrix = show $ unDPSMatrix smatrix


plus :: SHMatrix x y L0 (SDouble Diff) Double s1 -> SHMatrix x y L1 (SDouble Diff) Double s2 -> SHMatrix x y L1 (SDouble Diff) Double (s1 +++ s2)
plus m0 m2 =
  SHMatrix_UNSAFE $ (unSHMatrix m0) + (unSHMatrix m2)