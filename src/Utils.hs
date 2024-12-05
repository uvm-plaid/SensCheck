{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Data.Matrix qualified as Matrix
import Sensitivity (DPSMatrix (unDPSMatrix), SDouble (unSDouble), SMatrix (unSMatrix), SList (..))
import GHC.TypeLits.Singletons
import GHC.TypeNats ( withSomeSNat )
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Control.Monad (replicateM)

-- General utilty functions that might be used by generated code

toDoubleMatrix :: SMatrix m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix m = unSDouble <$> unSMatrix m

toDoubleMatrix' :: DPSMatrix x y m1 (SDouble m2) s -> Matrix.Matrix Double
toDoubleMatrix' m = unSDouble <$> unDPSMatrix m

-- | Transform f requiring a KnownNat to one that doesn't using the passed Natural number
-- Credit: @Jannis FP discord
withKnownNat :: (forall n . KnownNat n => r) -> Natural -> r
withKnownNat f n = withSomeSNat n $ \(SNat :: SNat n) -> f @n

withKnownNat2 :: (forall n1 n2 . (KnownNat n1, KnownNat n2) => r) -> Natural -> Natural -> r
withKnownNat2 f n1 n2 = withSomeSNat n1 $ \(SNat :: SNat n1) -> 
    withSomeSNat n2 $ \(SNat :: SNat n2) ->
      f @n1 @n2

-- Useful for generating lists of the same size with quickcheck
data SameSizedSLists m t senv = SameSizedSLists (SList m t senv) (SList m t senv) deriving (Show)

-- Have quickcheck generate lists of the same size

instance (Arbitrary (t senv)) => Arbitrary (SameSizedSLists m t senv) where
  arbitrary = do
    size <- arbitrary
    l1 <- replicateM size arbitrary
    l2 <- replicateM size arbitrary
    pure $ SameSizedSLists (SList_UNSAFE l1) (SList_UNSAFE l2)
