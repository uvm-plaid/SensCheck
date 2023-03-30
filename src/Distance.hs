{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Distance where

import Data.Matrix qualified as Matrix
import Sensitivity (CMetric (..), DPSMatrix (DPSMatrix_UNSAFE), NMetric (..), SDouble, SList (SList_UNSAFE, unSList), SMatrix (SMatrix_UNSAFE), SPair (P_UNSAFE), unSDouble)
import Utils (toDoubleMatrix, toDoubleMatrix')

class Distance a where
  distance :: a -> a -> Double

-- Base Types
instance Distance (SDouble Diff senv) where
  distance a b = absdist (unSDouble a) (unSDouble b)

instance Distance (SDouble Disc senv) where
  distance a b = discdist (unSDouble a) (unSDouble b)

-- Container Types

instance Distance (stype senv) => Distance (SList L2 stype senv) where
  distance (SList_UNSAFE a) (SList_UNSAFE b) = l2norm $ uncurry distance <$> zip a b

instance (Distance (stype1 senv), Distance (stype2 senv)) => Distance (SPair L2 stype1 stype2 senv) where
  distance (P_UNSAFE (al, ar)) (P_UNSAFE (bl, br)) = l2norm [distance al bl, distance ar br]

instance Distance (stype senv) => Distance (SMatrix L2 stype senv) where
  distance (SMatrix_UNSAFE a) (SMatrix_UNSAFE b) = l2norm (uncurry distance <$> zip (Matrix.toList a) (Matrix.toList b))

instance Distance (stype senv) => Distance (DPSMatrix x y L2 stype senv) where
  distance (DPSMatrix_UNSAFE a) (DPSMatrix_UNSAFE b) = l2norm (uncurry distance <$> zip (Matrix.toList a) (Matrix.toList b))

-- TODO more instances.

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

-- L2 norm of a Matrix
-- TODO remove?
norm_2 :: Floating n => Matrix.Matrix n -> n
norm_2 m = sqrt $ foldr (\x acc -> acc + abs x ** 2) 0 m
