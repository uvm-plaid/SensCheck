{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import AnnotatedExternalLibrary (add_dependently_typed_matrix_solo, add_matrix_solo, add_pair_solo, solo_mixed_types, solo_mixed_types_mult, solo_plus, solo_plus_incorrect)
import GHC.TypeLits (KnownNat)
import Sensitivity
import TH (genMainQuickCheck)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils

f = add_dependently_typed_matrix_solo @2 @4
$(genMainQuickCheck "tests" [
  -- 'solo_plus,
  -- 'add_pair_solo,
  -- 'f,
  'solo_mixed_types,
  'solo_mixed_types_mult
  ])

$(genMainQuickCheck "failing_tests" ['add_matrix_solo, 'solo_plus_incorrect])

main :: IO ()
main = do
  putStrLn "\n\nThese tests are expected to pass:"
  tests
  putStrLn "\n\n=================================="
  putStrLn "These tests are expected to fail:\n\n"
  failing_tests
