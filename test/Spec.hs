{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import AnnotatedExternalLibrary (add_dependently_typed_matrix_solo, add_matrix_solo, solo_plus, solo_plus_incorrect)
import DistanceFunctions
import GHC.TypeLits (KnownNat)
import Sensitivity
import TH (genMainQuickCheck)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils

f = add_dependently_typed_matrix_solo @2 @4
$(genMainQuickCheck "tests" ['solo_plus, 'add_matrix_solo, 'f, 'solo_plus_incorrect])

main :: IO ()
main = tests
