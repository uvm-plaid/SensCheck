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
import TH (sensCheck, sensProperty)
import Test.QuickCheck (quickCheck, withMaxSuccess, generate, choose, forAll)
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix

$( sensCheck
    "passingTests"
    [ 'Correct.solo_double
    , 'Correct.solo_plus
    , 'Correct.add_pair_solo
    , 'Correct.solo_mixed_types
    , 'Correct.solo_mixed_types_mult
    ]
 )


$( sensCheck
    "failingTests"
    [ 'Incorrect.add_pair_solo1
    , 'Incorrect.solo_double1
    ]
 )

-- The below uses the sensProperty function to generate the property
-- Which the sensCheck function uses but these functions have slightly different bootstraping

$( singleton <$> sensProperty 'DpMinst.clippedGrad)

sensCheckDPClippedGrad = do
  net0 <- evalRandIO randomMnist
  quickCheck $ withMaxSuccess 100 
    (\case SameSizedSLists trainingRows1 trainingRows2 -> 
            clippedGradProp trainingRows1 trainingRows2 net0
    )

$( singleton <$> sensProperty 'SensStaticHMatrix.plus)
$( singleton <$> sensProperty 'SensStaticHMatrix.mult)

testStaticPlus =
  quickCheck
    (forAll
      (SensStaticHMatrix.genFour
         (\m1 m2 m3 m4 -> pure (plusProp m1 m2 m3 m4))
      )
      id
    )

testStaticScalarMult =
  quickCheck
    (forAll
      (SensStaticHMatrix.genScalar
         (\proxy m1 m2 -> pure (scalarMultProp m1 m2))
      )
      id
    )

testStaticMult =
  quickCheck
    (forAll
      (SensStaticHMatrix.genFourMult
         (\m1 m2 m3 m4 -> pure (multProp m1 m2 m3 m4))
      )
      id
    )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pass"] -> pass
    ["fail"] -> fail
    _ -> do
      putStrLn "Defaulting to running all tests."
      putStrLn "To run specific suite run as stack test --test-arguments=\"pass|fail\""
      pass
      fail
  where
    pass = do
      putStrLn "\n\nThese tests are expected to pass:"
      testStaticPlus
      passingTests
      sensCheckDPClippedGrad
    fail = do
      putStrLn "\nThese tests are expected to fail:\n\n"
      failingTests
