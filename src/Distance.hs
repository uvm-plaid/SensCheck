{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Distance where

import DistanceFunctions
import Sensitivity (CMetric (..), NMetric (..), SDouble, SList, SMatrix, unSDouble)
import Utils (toDoubleMatrix)

class Distance a where
  distance :: a -> a -> Double

instance Distance [i] where
  distance a b = 3

instance Distance Integer where
  distance a b = 3

instance Distance (SDouble Diff s) where
  distance a b = absdist (unSDouble a) - absdist (unSDouble b)

instance Distance (SDouble Disc s) where
  distance a b = 4

instance Distance (SList L2 i s) where
  distance a b = 4

instance Distance (SList L1 i s) where
  distance a b = 4

instance Distance (SMatrix L2 (SDouble Diff) s) where
  distance a b = l2dist (toDoubleMatrix a) - l2dist (toDoubleMatrix b)

instance Distance (SMatrix L1 i s) where
  distance a b = 4

-- TODO rest of them
-- Can we avoid defin
