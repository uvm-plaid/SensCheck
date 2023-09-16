{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import GHC.TypeLits (KnownNat)
import Sensitivity
import System.Environment (getArgs)
import TH (genMainQuickCheck)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils
import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect

add_dependently_typed_matrix_fixed = Correct.add_dependently_typed_matrix_solo @3 @4

add_dependently_typed_matrix_incorrect = Incorrect.add_dependently_typed_matrix_solo1 @3 @4

$( genMainQuickCheck
    "passing_tests"
    [ 'Correct.solo_double
    , 'Correct.solo_plus
    , 'Correct.add_pair_solo
    , 'add_dependently_typed_matrix_fixed
    , 'Correct.solo_mixed_types
    , 'Correct.solo_mixed_types_mult
    ]
 )

$( genMainQuickCheck
    "failing_tests"
    [ 'Incorrect.add_pair_solo1
    , 'add_dependently_typed_matrix_incorrect
    , 'Incorrect.solo_double1
    ]
 )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pass"] -> putStrLn "\n\nThese tests are expected to pass:" *> passing_tests
    ["fail"] -> putStrLn "These tests are expected to fail:\n\n" *> failing_tests
    otherwise -> putStrLn "Run as stack test --test-arguments=\"pass|fail\""
