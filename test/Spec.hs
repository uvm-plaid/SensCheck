{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

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

f = add_dependently_typed_matrix_solo @2 @4

$( genMainQuickCheck
    "tests"
    [ 'solo_plus
    , 'add_pair_solo
    , 'f
    , 'solo_mixed_types
    , 'solo_mixed_types_mult
    ]
 )

$( singleton <$> genProp 'DpMinst.clippedGrad)

sensCheckDPClippedGrad = do
  net0 <- evalRandIO randomMnist
  quickCheck $ withMaxSuccess 100 (\case SameSizedSLists trainingRows1 trainingRows2 -> clippedGrad_prop trainingRows1 trainingRows2 $! net0)

$(genMainQuickCheck "failing_tests" ['add_matrix_solo, 'solo_plus_incorrect])

main :: IO ()
main = do
  putStrLn "\n\nThese tests are expected to pass:"
  tests
  sensCheckDPClippedGrad
  putStrLn "\n\n=================================="

  putStrLn "These tests are expected to fail:\n\n"
  failing_tests
