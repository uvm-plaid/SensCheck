module Utils where
import Sensitivity (SMatrix (unSMatrix), SDouble (unSDouble))
import qualified Data.Matrix as Matrix
-- General utilty functions that might be used by generated code

toDoubleMatrix :: SMatrix m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix m = unSDouble <$> unSMatrix m