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

module Sensitivity where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Matrix qualified as Matrix
import Data.Proxy
import GHC.TypeLits (Nat)
import GHC.TypeLits qualified as TL
import GHC.TypeNats (KnownNat)
import Test.QuickCheck (Arbitrary, Positive (Positive))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Modifiers (Positive)
import Prelude
import Prelude qualified as P

type Source = TL.Symbol -- sensitive data sources
data Sensitivity = InfSens | NatSens TL.Nat -- sensitivity values
type SEnv = [(Source, Sensitivity)] -- sensitivity environments

data NMetric = Diff | Disc deriving (Show) -- distance metrics for numeric types
newtype SDouble (m :: NMetric) (s :: SEnv) = D_UNSAFE {unSDouble :: Double}

data CMetric = L1 | L2 | LInf deriving (Show) -- metrics for compound types
newtype SPair (m :: CMetric) (f1 :: SEnv -> *) (f2 :: SEnv -> *) (s :: SEnv) = P_UNSAFE {unSPair :: (f1 s, f2 s)} deriving (Show)
type L1Pair = SPair L1 -- \$⊗$-pairs in Fuzz
type L2Pair = SPair L2 -- Not in Fuzz
type LInfPair = SPair LInf -- \$\&$-pairs in Fuzz

newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE {unSList :: [f s]}
type L1List = SList L1 -- \$τ␣‹list›$ in Fuzz
type L2List = SList L2 -- Not in Fuzz
type LInfList = SList LInf -- \$τ␣‹alist›$ in Fuzz

type family IsLT (o :: Ordering) :: Bool where
  IsLT 'LT = 'True
  IsLT _ = 'False

type family IsEQ (o :: Ordering) :: Bool where
  IsEQ 'EQ = 'True
  IsEQ _ = 'False

type family Cond (b :: Bool) (x :: a) (y :: a) where
  Cond 'True x _ = x
  Cond 'False _ y = y

type family (+++) (s1 :: SEnv) (s2 :: SEnv) :: SEnv where
  '[] +++ s2 = s2
  s1 +++ '[] = s1
  ('(o, NatSens n1) ': s1) +++ ('(o, NatSens n2) ': s2) = '(o, NatSens (n1 TL.+ n2)) ': (s1 +++ s2)
  ('(o1, NatSens n1) ': s1) +++ ('(o2, NatSens n2) ': s2) =
    Cond
      (IsLT (TL.CmpSymbol o1 o2))
      ('(o1, NatSens n1) ': (s1 +++ ('(o2, NatSens n2) ': s2)))
      ('(o2, NatSens n2) ': (('(o1, NatSens n1) ': s1) +++ s2))

type family ScaleSens (s :: SEnv) (n :: TL.Nat) :: SEnv where
  ScaleSens '[] _ = '[]
  ScaleSens ('(o, NatSens n2) ': s) n1 = '(o, NatSens (n1 TL.* n2)) ': ScaleSens s n1

type family MaxSens (s :: SEnv) :: TL.Nat where
  MaxSens '[] = 0
  MaxSens ('(_, NatSens n) ': s) = MaxNat n (MaxSens s)

type family MaxNat (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  MaxNat n1 n2 = Cond (n1 TL.<=? n2) n2 n1

type family TruncateNat (n1 :: TL.Nat) (n2 :: TL.Nat) :: TL.Nat where
  TruncateNat _ 0 = 0
  TruncateNat n _ = n

type family TruncateSens (n :: TL.Nat) (s :: SEnv) :: SEnv where
  TruncateSens _ '[] = '[]
  TruncateSens n1 ('(o, NatSens n2) ': s) =
    '(o, NatSens (TruncateNat n1 n2)) ': TruncateSens n1 s

-- >>> :kind! TruncateSens 2 '[ '("t.csv", 'NatSens 3)]
-- TruncateSens 2 '[ '("t.csv", 'NatSens 3)] :: [(Source,
--                                                Sensitivity)]
-- = '[ '("t.csv", 'NatSens 2)]

type family TruncateInf (s :: SEnv) :: SEnv where
  TruncateInf '[] = '[]
  TruncateInf ('(o, _) ': s) = '(o, InfSens) ': TruncateInf s

-- Unused
type family JoinSens (s1 :: SEnv) (s2 :: SEnv) :: SEnv where
  JoinSens '[] s2 = s2
  JoinSens s1 '[] = s1
  JoinSens ('(o, NatSens n1) ': s1) ('(o, NatSens n2) ': s2) =
    '(o, NatSens (MaxNat n1 n2)) ': (JoinSens s1 s2)
  JoinSens ('(o1, NatSens n1) ': s1) ('(o2, NatSens n2) ': s2) =
    Cond
      (IsLT (TL.CmpSymbol o1 o2))
      ('(o1, NatSens n1) ': (JoinSens s1 ('(o2, NatSens n2) ': s2)))
      ('(o2, NatSens n2) ': (JoinSens ('(o1, NatSens n1) ': s1) s2))

-----------------------------------------------------------------------------------------------------
-- New code to Solo

-- Required for quickCheck
instance Show (SDouble Diff s) where
  show sdouble = "SDouble Diff '[] " <> show (unSDouble sdouble)

instance Arbitrary (SDouble Diff s) where
  arbitrary = D_UNSAFE <$> arbitrary @Double

instance (Arbitrary (stype1 senv), Arbitrary (stype2 senv)) => Arbitrary (SPair metric stype1 stype2 senv) where
  arbitrary = do
    s1 <- arbitrary @(stype1 senv)
    s2 <- arbitrary @(stype2 senv)
    pure $ P_UNSAFE (s1, s2)

-- Similar to SList we need a custom data type
newtype SMatrix (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SMatrix_UNSAFE {unSMatrix :: Matrix.Matrix (f s)}

-- A Matrix of SDoubles with L2 Metrix
type SDoubleMatrixL2 s = SMatrix L2 (SDouble Diff) s

instance (Show (f s)) => Show (SMatrix m f s) where
  show smatrix = show $ unSMatrix smatrix

instance (forall senv. Arbitrary (innerType senv)) => Arbitrary (SMatrix cmetric innerType s1) where
  arbitrary = do
    -- Generate an arbitrary number of rows and columns greater than 0
    (Positive row) <- arbitrary @(Positive Int)
    (Positive col) <- arbitrary @(Positive Int)
    -- Generate a list of arbitrary elements of size row * col
    elems <- replicateM (row * col) arbitrary
    -- Convert to Matrix
    pure $ SMatrix_UNSAFE $ Matrix.fromList row col elems

-- Dependently typed Matrix that is better for automatically generating props
-- Credit to https://hackage.haskell.org/package/matrix-static-0.3/docs/src/Data.Matrix.Static.html#Matrix
newtype DPSMatrix (x :: Nat) (y :: Nat) (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = DPSMatrix_UNSAFE {unDPSMatrix :: Matrix.Matrix (f s)}

-- A Matrix of DPSDoubles with L2 Metrix
type DPSDoubleMatrixL2 x y s = DPSMatrix x y L2 (SDouble Diff) s

instance (forall senv. Arbitrary (innerType senv), KnownNat x, KnownNat y) => Arbitrary (DPSMatrix x y cmetric innerType s1) where
  arbitrary = do
    -- Note that I'm not generating matrices of aribitary row and column size unlike SMatrix. This becomes useful in generating matrices that require rows and cols to match.
    let row = TL.natVal @x Proxy
        col = TL.natVal @y Proxy
    -- Generate a list elements of size row * col
    elems <- replicateM (fromInteger (row * col)) arbitrary
    -- Convert to Matrix
    pure $ DPSMatrix_UNSAFE $ Matrix.fromList (fromInteger row) (fromInteger col) elems

instance (Show (f s)) => Show (DPSMatrix x y m f s) where
  show smatrix = show $ unDPSMatrix smatrix
