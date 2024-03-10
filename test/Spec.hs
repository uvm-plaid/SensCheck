{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE ScopedTypeVariables #-}

import AnnotatedExternalLibrary (add_dependently_typed_matrix_solo, add_matrix_solo, add_pair_solo, solo_mixed_types, solo_mixed_types_mult, solo_plus, solo_plus_incorrect)
import Control.Monad
import Control.Monad.Random
import Debug.Trace qualified as Debug
import Distance
import DpMinst (SGradients (..), flattenGrads, randomMnist, SameSizedSLists (SameSizedSLists))
import DpMinst qualified
import GHC.TypeLits (KnownNat)
import Sensitivity
import TH (genMainQuickCheck, genProp)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix

add_dependently_typed_matrix_incorrect = Incorrect.add_dependently_typed_matrix_solo1 @3 @4

sensStaticHMatrixPlus = SensStaticHMatrix.plus @3 @4 @L1 @Diff
sensStaticHMatrixMult = SensStaticHMatrix.mult @3 @4 @5 @L1 @Diff


$( genMainQuickCheck
    "passing_tests"

    [ 'Correct.solo_double
    , 'Correct.solo_plus
    , 'Correct.add_pair_solo
    , 'Correct.solo_mixed_types
    , 'Correct.solo_mixed_types_mult
    , 'sensStaticHMatrixPlus
    , 'sensStaticHMatrixMult
    ]
 )

-- Need to initialize a random network in IO so manually writing the test however the property is still generated
$( singleton <$> genProp 'DpMinst.clippedGrad)

sensCheckDPClippedGrad = do
  net0 <- evalRandIO randomMnist
  quickCheck $ withMaxSuccess 100 (\case SameSizedSLists trainingRows1 trainingRows2 -> clippedGrad_prop trainingRows1 trainingRows2 net0)

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
    ["pass"] -> do
      putStrLn "\n\nThese tests are expected to pass:"
      passing_tests
      sensCheckDPClippedGrad
    ["fail"] -> putStrLn "These tests are expected to fail:\n\n" *> failing_tests
    otherwise -> putStrLn "Run as stack test --test-arguments=\"pass|fail\""
