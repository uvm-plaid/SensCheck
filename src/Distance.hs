{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- TODO the scaled sens stuff might have needed this. Might want to remove it eventually
-- {-# LANGUAGE UndecidableInstances #-}

module Distance where

import Data.Matrix qualified as Matrix
import Data.Singletons (SingI (..))
import Grenade
import Grenade.Core.Shape
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra.Data qualified as LAD
import Numeric.LinearAlgebra.Static (Sized (unwrap))
import Numeric.LinearAlgebra.Static qualified as LAS
import Sensitivity (CMetric (..), DPSMatrix (DPSMatrix_UNSAFE), NMetric (..), SDouble (D_UNSAFE), SList (SList_UNSAFE, unSList), SMatrix (SMatrix_UNSAFE), SPair (P_UNSAFE), unSDouble, ScaleSens)
import Utils (toDoubleMatrix, toDoubleMatrix')
import SensStaticHMatrix (SensStaticHMatrix (SensStaticHMatrixUNSAFE))
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix
import qualified Numeric.LinearAlgebra.Static as Static
import GHC.TypeLits (KnownNat)

class Distance a where
  distance :: a -> a -> Double

-- Base Types
instance Distance (SDouble Diff senv) where
  distance a b = absdist (unSDouble a) (unSDouble b)

instance Distance (SDouble Disc senv) where
  distance a b = discdist (unSDouble a) (unSDouble b)

-- Container Types

instance Distance (stype senv) => Distance (SList L2 stype senv) where
  distance (SList_UNSAFE a) (SList_UNSAFE b) = l2dist a b

instance Distance (stype senv) => Distance (SList L1 stype senv) where
  distance (SList_UNSAFE a) (SList_UNSAFE b) = l1dist a b

-- For 2 tuples with elements: (a_1, b_1) and (a_2, b_2)
-- We need to take the following distances
-- d(d(a_1, a_2), d(b_1, b_2))

instance (Distance (stype1 senv), Distance (stype2 senv)) => Distance (SPair L2 stype1 stype2 senv) where
  distance (P_UNSAFE (al, ar)) (P_UNSAFE (bl, br)) = l2norm [distance al bl, distance ar br]

instance Distance (stype senv) => Distance (SMatrix L2 stype senv) where
  distance (SMatrix_UNSAFE a) (SMatrix_UNSAFE b) = l2dist (Matrix.toList a) (Matrix.toList b)

instance Distance (stype senv) => Distance (DPSMatrix x y L2 stype senv) where
  distance (DPSMatrix_UNSAFE a) (DPSMatrix_UNSAFE b) = l2norm (uncurry distance <$> zip (Matrix.toList a) (Matrix.toList b))

instance forall x y nmetric senv. (KnownNat x, KnownNat y, Distance (SDouble nmetric senv)) => Distance (SensStaticHMatrix x y L2 nmetric senv) where
  distance (SensStaticHMatrixUNSAFE a) (SensStaticHMatrixUNSAFE b) = l2norm (
    uncurry distance <$>
      zip
        (D_UNSAFE @nmetric @senv <$> concat (HMatrix.toLists $ Static.unwrap a))
        (D_UNSAFE @nmetric @senv <$> concat (HMatrix.toLists $ Static.unwrap b))
      )

instance forall x y nmetric senv. (KnownNat x, KnownNat y, Distance (SDouble nmetric senv)) => Distance (SensStaticHMatrix x y L1 nmetric senv) where
  distance (SensStaticHMatrixUNSAFE a) (SensStaticHMatrixUNSAFE b) = l1norm (
    uncurry distance <$>
      zip
        (D_UNSAFE @nmetric @senv <$> concat (HMatrix.toLists $ Static.unwrap a))
        (D_UNSAFE @nmetric @senv <$> concat (HMatrix.toLists $ Static.unwrap b))
      )


-- Distance Functions

absdist :: Floating n => n -> n -> n
absdist x y = abs $ x - y

discdist :: (Eq a1, Floating a2) => a1 -> a1 -> a2
discdist x y = if x == y then 0 else 1

l1norm :: (Traversable f, Floating n) => f n -> n
l1norm l = sum $ abs <$> l

l2norm :: (Traversable f, Floating n) => f n -> n
l2norm l = sqrt $ foldl (\acc x -> x ** 2 + acc) 0 l

infnorm :: (Traversable f, Ord n) => f n -> n
infnorm = maximum

-- Distances defined in terms of norms

l1dist :: Distance a => [a] -> [a] -> Double
l1dist x y = l1norm $ uncurry distance <$> zip x y

l2dist :: Distance a => [a] -> [a] -> Double
l2dist x y = l2norm $ uncurry distance <$> zip x y

-- L2 norm of a Matrix
-- TODO remove?
norm_2 :: Floating n => Matrix.Matrix n -> n
norm_2 m = sqrt $ foldr (\x acc -> acc + abs x ** 2) 0 m
