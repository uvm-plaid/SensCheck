module Utils where

import Data.Matrix qualified as Matrix
import Sensitivity (DPSMatrix (unDPSMatrix), SDouble (unSDouble), SMatrix (unSMatrix))

-- General utilty functions that might be used by generated code

toDoubleMatrix :: SMatrix m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix m = unSDouble <$> unSMatrix m

toDoubleMatrix' :: DPSMatrix x y m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix' m = unSDouble <$> unDPSMatrix m
