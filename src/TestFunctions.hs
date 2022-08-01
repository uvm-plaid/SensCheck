{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestFunctions where

-- Some functions to test the code generator on
import Sensitivity
import Verifier (SMatrix)

allTypes ::
  SDouble Diff s1 ->
  SDouble Disc s2 ->
  SMatrix L2 (SDouble Diff) s3 ->
  SMatrix L2 (SDouble Disc) s4 ->
  SMatrix L1 (SDouble Diff) s5 ->
  SMatrix L1 (SDouble Disc) s6 ->
  SList L2 (SDouble Diff) s7 ->
  SList L1 (SDouble Diff) s8 ->
  SList L2 (SDouble Disc) s10 ->
  SList L2 (SDouble Disc) s11 ->
  SDouble Diff (s1 +++ s2)
allTypes = undefined
