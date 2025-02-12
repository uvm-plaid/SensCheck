{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AnnotatedExternalLibrary where

import Control.Monad (replicateM)
import Data.Matrix qualified as Matrix
import Debug.Trace (trace)
import Distance
import Sensitivity (CMetric (..), DPSDoubleMatrixL2, DPSMatrix (DPSMatrix_UNSAFE, unDPSMatrix), NMetric (..), SDouble (..), SDoubleMatrixL2, SEnv, SMatrix (SMatrix_UNSAFE, unSMatrix), SPair (P_UNSAFE), type (+++), SList (unSList), JoinSens, ScaleSens, MaxNat, TruncateInf)
import Utils
import qualified GHC.TypeLits as TL
import Data.Data (Proxy (..))
import Primitives
import StdLib
import Test.QuickCheck (Gen, quickCheck, forAll, again, withMaxSuccess)
import SFunction
import GHC.Base (Type)
import qualified Debug.Trace as Debug

{- | This Module simulates a developer re-exposing "unsafe" external libraries as solo annotated functions
 Also includes examples of manually generated props
-}

-- This is a function from an external library for example hmatrix
unsafe_plus :: Double -> Double -> Double
unsafe_plus a b = a + b

-- Assert that |x1-x2| + |y1 - y2| <= |f(x1,x2) - f(x2, y2)|
unsafe_unsafe_plus_prop :: Double -> Double -> Double -> Double -> Bool
unsafe_unsafe_plus_prop x1 y1 x2 y2 =
  let d1 = abs (x1 - x2)
      d2 = abs (y1 - y2)
      dout = abs $ unsafe_plus x1 y2 - unsafe_plus x2 y2
   in dout <= d1 + d2

-- This is a "developer" who's reexposed it with sensitivity annotations. But is it right? We will test that.
solo_plus :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
solo_plus a b = D_UNSAFE $ unsafe_plus (unSDouble a) (unSDouble b)

-- This is a "developer" who's reexposed but implemented incorrectly.
solo_plus_incorrect :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff s1
solo_plus_incorrect a b = D_UNSAFE $ unsafe_plus (unSDouble a) (unSDouble b)

solo_plus_prop :: SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> SDouble Diff '[] -> Bool
solo_plus_prop a1 a2 b1 b2 =
  let d1 = abs $ unSDouble a1 - unSDouble a2
      d2 = abs $ unSDouble b1 - unSDouble b2
      dout = abs $ unSDouble (solo_plus a1 b1) - unSDouble (solo_plus a2 b2)
   in dout <= d1 + d2 + 0.000000001

-- Without Solo - L2 sensitivity of matrix addition
unsafe_add :: Matrix.Matrix Double -> Matrix.Matrix Double -> Matrix.Matrix Double
unsafe_add m1 m2 = m1 + m2

prop_unsafe_add ::
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Matrix.Matrix Double ->
  Bool
prop_unsafe_add a1 a2 b1 b2 =
  let d1 = norm_2 (a1 - a2) -- L2 distance between first arguments
      d2 = norm_2 (b1 - b2) -- L2 distance between second arguments
      -- L2 distance between two outputs
      dout = norm_2 $ unsafe_add a1 b1 - unsafe_add a2 b2
   in dout <= d1 + d2 + 0.000000001

-- With Solo - L2 sensitivity of matrix addition
add_matrix_solo :: SDoubleMatrixL2 s1 -> SDoubleMatrixL2 s2 -> SDoubleMatrixL2 (s1 +++ s2)
add_matrix_solo m1 m2 =
  SMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unSMatrix m1) + (unSDouble <$> unSMatrix m2)

prop_safe_add_solo ::
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  SDoubleMatrixL2 '[] ->
  Bool
prop_safe_add_solo a1 a2 b1 b2 =
  let d1 = norm_2 $ toDoubleMatrix a1 - toDoubleMatrix a2 -- L2 distance between first arguments
      d2 = norm_2 $ toDoubleMatrix b1 - toDoubleMatrix b2
      -- L2 distance between two outputs
      dout = norm_2 $ toDoubleMatrix (add_matrix_solo a1 b1) - toDoubleMatrix (add_matrix_solo a2 b2)
   in dout <= d1 + d2 + 0.000000001

add_dependently_typed_matrix_solo :: DPSDoubleMatrixL2 x y s1 -> DPSDoubleMatrixL2 x y s2 -> DPSDoubleMatrixL2 x y (s1 +++ s2)
add_dependently_typed_matrix_solo m1 m2 =
  DPSMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unDPSMatrix m1) + (unSDouble <$> unDPSMatrix m2)

add_pair_solo :: SPair L2 (SDouble Diff) (SDouble Diff) s1 -> SPair L2 (SDouble Diff) (SDouble Diff) s2 -> SPair L2 (SDouble Diff) (SDouble Diff) (s1 +++ s2)
add_pair_solo (P_UNSAFE (D_UNSAFE al, D_UNSAFE ar)) (P_UNSAFE (D_UNSAFE bl, D_UNSAFE br)) = P_UNSAFE (D_UNSAFE $ al + bl, D_UNSAFE $ ar + br)

-- Examples with mixed types

-- This is an example of mixed typed function to show how Solo can handle sensitive and non-sensitive types
solo_mixed_types :: SDouble Diff s1 -> SDouble Diff s2 -> Bool -> SDouble Diff (JoinSens s1 s2)
solo_mixed_types a b chooseA = D_UNSAFE $ if chooseA then unSDouble a else unSDouble b

solo_double :: SDouble Diff s1 -> SDouble Diff (s1 +++ s1)
solo_double a = D_UNSAFE $ unSDouble a + unSDouble a

-- Sensitive identity function
sid :: a s -> a (ScaleSens s 1)
sid = cong (eq_sym scale_unit)

smapId :: forall m b s2. (SPrimitive b) => SList m b s2 -> SList m b s2
smapId = cong scale_unit . smap @1 sid

-- Example demonstrating a higher order function that is applied
slistAddConst :: forall m s. Double -> SList m (SDouble Diff) s -> SList m (SDouble Diff) s
slistAddConst const = cong scale_unit . smap @1 (\x -> D_UNSAFE $ unSDouble x + const)

-- | Functions need to be instantiated with a concete type (since there is an infite number of types)
-- Ideally we could do this inline this but TH seems to have an issue
slistAdd42 = slistAddConst @L2 42

smapIdDoubles = smapId @L2 @(SDouble Diff)

-- More interesting is if we support higher order functions like QuickCheck.Function described in Shrinking and showing functions: (functional pearl) Koen Claessen https://dl.acm.org/doi/10.1145/2430532.2364516
-- We have a solution similar to QuickCheck.Function but differs since we do care about relations of intermediate types
-- The same restriction applies on making types concrete

-- The user writes this making it monomorphic except for any SEnv
smapSDoubleDiffL2 :: (forall (s1 :: SEnv).  SDouble Diff s1 -> SDouble Diff (ScaleSens s1 1)) -> SList L2 (SDouble Diff) w -> SList L2 (SDouble Diff) (ScaleSens w 1)
smapSDoubleDiffL2 = smap @1 @(SDouble Diff) @(SDouble Diff) @_ @L2

-- sensCheck generates this
smapSDoubleProp :: Double -> SList L2 (SDouble Diff) s2 -> SList L2 (SDouble Diff) s2 -> Bool
smapSDoubleProp randomNumber xs ys =
  let distIn = distance xs ys
      distOut = distance (smapSDoubleDiffL2 (sfunctionTable (Proxy @1) randomNumber) xs) (smapSDoubleDiffL2 (sfunctionTable (Proxy @1) randomNumber) ys)
  in distOut <= distIn

-- They may also test it for other concrete types
smapSDoubleDiscL2 = smap @1 @(SDouble Disc) @(SDouble Disc) @_ @L2

-- TODO missing instances but just to show how it might be used
-- smapSDoubleDiscProp :: Double -> SList L2 (SDouble Disc) s2 -> SList L2 (SDouble Disc) s2 -> Bool
-- smapSDoubleDiscProp randomNumber xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smapSDoubleDiscL2 (sfunctionTable3 (Proxy @1) randomNumber) xs) (smapSDoubleDiscL2 (sfunctionTable3 (Proxy @1) randomNumber) ys)
--   in distOut <= distIn

smapMain :: IO ()
smapMain = do
  quickCheck smapSDoubleProp
  -- quickCheck $ smapSDoubleDiscProp

-- sfoldr_ :: forall fn_sens1 fn_sens2 t1 t2 cm s3 s4 s5 . (SPrimitive t1, SPrimitive t2) =>
sfoldrSDoubleDiffL2 = sfoldr @1 @1 @(SDouble Diff) @(SDouble Diff) @L2
sfoldrSDoubleDiffL1 = sfoldr @1 @1 @(SDouble Diff) @(SDouble Diff) @L1

sfoldrSDoubleDiscL2 = sfoldr @1 @1 @(SDouble Disc) @(SDouble Disc) @L2

-- What should the property be? for sfoldr @1 @1 (generatedFunction)
-- s4 * (MaxNat 1 1) +++ TruncateInf s5
-- s4 * 1 +++ TruncateInf s5
-- s4 + TruncateInf s5
-- s4 + s5 Not infinity
-- The distance for s5 shouldn't be considered since we use the same acc?
-- Thereforward we shoud use an unscaled distance
sfoldrSDoubleDiffL2Prop :: forall s4 s5. Double -> SList L2 (SDouble Diff) s4 -> SList L2 (SDouble Diff) s4 -> SDouble Diff s5 -> Bool
sfoldrSDoubleDiffL2Prop randomNumber xs ys init =
  let distIn = distance xs ys
      distOut = distance (sfoldrSDoubleDiffL2 (sfunctionTable2 (Proxy @1) (Proxy @1) randomNumber) init xs) (sfoldrSDoubleDiffL2 (sfunctionTable2 (Proxy @1) (Proxy @1) randomNumber) init ys)
  in distOut <= distIn + 0.00000001

sfoldrSDoubleDiffL2HighSens = sfoldr @2 @2 @(SDouble Diff) @(SDouble Diff) @L2

-- What should the property be? for sfoldr @2 @2 (generatedFunction)
-- s4 * (MaxNat 2 2) +++ TruncateInf s5
-- s4 * 2
-- The distance of the output should be twice the distance of the input
sfoldrSDoubleDiscPropHighSens :: forall s4 s5. Double -> SList L2 (SDouble Diff) s4 -> SList L2 (SDouble Diff) s4 -> SDouble Diff s5 -> Bool
sfoldrSDoubleDiscPropHighSens randomNumber xs ys init =
  let distIn = distance xs ys
      distOut = distance (sfoldrSDoubleDiffL2HighSens (sfunctionTable2 (Proxy @2) (Proxy @2) randomNumber) init xs) (sfoldrSDoubleDiffL2HighSens (sfunctionTable2 (Proxy @2) (Proxy @2) randomNumber) init ys)
  in distOut <= 2 * distIn + 0.00000001

sfoldrMain :: IO ()
sfoldrMain = do
  putStrLn "Testing sfoldr"
  quickCheck $ withMaxSuccess 1000 (\random (SameSizedSLists l1 l2) -> sfoldrSDoubleDiffL2Prop random l1 l2)
  putStrLn "Testing 2-sens sfoldr"
  quickCheck (\random (SameSizedSLists l1 l2) -> sfoldrSDoubleDiscPropHighSens random l1 l2)

smapSDoubleDiffL2HighSens = smap @300 @(SDouble Diff) @(SDouble Diff) @_ @L2
