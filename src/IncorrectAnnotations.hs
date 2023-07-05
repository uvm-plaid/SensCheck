{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds#-}

module IncorrectAnnotations where

import Control.Monad (replicateM)
import Data.Matrix qualified as Matrix
import Debug.Trace (trace)
import Distance
import Sensitivity (CMetric (..), DPSDoubleMatrixL2, DPSMatrix (DPSMatrix_UNSAFE, unDPSMatrix), NMetric (Diff), SDouble (..), SDoubleMatrixL2, SEnv, SMatrix (SMatrix_UNSAFE, unSMatrix), SPair (P_UNSAFE), type (+++), SList, JoinSens, ScaleSens)
import Utils
import qualified GHC.TypeLits as TL
import Data.Data (Proxy)

-- Incorrect sensitivity: Left out the s2 sensitivity
solo_plus1 :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff s1
solo_plus1 a b = D_UNSAFE $ unSDouble a  + unSDouble b

-- Implementation error multiplication instead of addition
solo_plus2 :: SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
solo_plus2 a b = D_UNSAFE $ unSDouble a * unSDouble b

-- Incorrect sensitivity: Left out the s2 sensitivity
add_pair_solo1 :: SPair L2 (SDouble Diff) (SDouble Diff) s1 -> SPair L2 (SDouble Diff) (SDouble Diff) s2 -> SPair L2 (SDouble Diff) (SDouble Diff) s1
add_pair_solo1 (P_UNSAFE (D_UNSAFE al, D_UNSAFE ar)) (P_UNSAFE (D_UNSAFE bl, D_UNSAFE br)) = P_UNSAFE (D_UNSAFE $ al + bl, D_UNSAFE $ ar + br)

-- Implementation error multiplication instead of addition
add_pair_solo2:: SPair L2 (SDouble Diff) (SDouble Diff) s1 -> SPair L2 (SDouble Diff) (SDouble Diff) s2 -> SPair L2 (SDouble Diff) (SDouble Diff) (s1 +++ s2)
add_pair_solo2 (P_UNSAFE (D_UNSAFE al, D_UNSAFE ar)) (P_UNSAFE (D_UNSAFE bl, D_UNSAFE br)) = P_UNSAFE (D_UNSAFE $ al * bl, D_UNSAFE $ ar * br)

-- Incorrect sensitivity: Left out the s2 sensitivity
add_dependently_typed_matrix_solo1 :: DPSDoubleMatrixL2 x y s1 -> DPSDoubleMatrixL2 x y s2 -> DPSDoubleMatrixL2 x y s2
add_dependently_typed_matrix_solo1 m1 m2 =
  DPSMatrix_UNSAFE $
    D_UNSAFE
      <$> (unSDouble <$> unDPSMatrix m1) + (unSDouble <$> unDPSMatrix m2)

