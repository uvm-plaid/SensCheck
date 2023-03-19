{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Distance where

import DistanceFunctions
import Sensitivity (CMetric (..), DPSMatrix, NMetric (..), SDouble, SList (unSList), SMatrix, SPair (P_UNSAFE), unSDouble)
import Utils (toDoubleMatrix, toDoubleMatrix')

class Distance a where
  distance :: a -> a -> Double

instance Distance (SDouble Diff s) where
  distance a b = absdist (unSDouble a) (unSDouble b)

instance Distance (SDouble Disc s) where
  distance a b = undefined

instance Distance (SList L2 (SDouble Diff) s) where
  distance a b = l2dist (unSDouble <$> unSList a) - l2dist (unSDouble <$> unSList b)

instance Distance (SPair L2 (SDouble Diff) (SDouble Diff) s) where
  distance (P_UNSAFE (al, ar)) (P_UNSAFE (bl, br)) = l1norm [unSDouble al - unSDouble ar, unSDouble bl - unSDouble br]

instance Distance (SMatrix L2 (SDouble Diff) s) where
  distance a b = l2dist (toDoubleMatrix a - toDoubleMatrix b)

instance Distance (DPSMatrix x y L2 (SDouble Diff) s) where
  distance a b = l2dist (toDoubleMatrix' a - toDoubleMatrix' b)

instance Distance (SMatrix L1 i s) where
  distance a b = undefined

-- TODO more instances.
-- TODO could better compose these typeclasses together better
