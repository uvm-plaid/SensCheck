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
import TH (sensCheck, genProp)
import Test.QuickCheck (quickCheck, withMaxSuccess, generate, choose, forAll)
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix

add_dependently_typed_matrix_incorrect = Incorrect.add_dependently_typed_matrix_solo1 @3 @4

sensStaticHMatrixPlus = SensStaticHMatrix.plus @3 @4 @L1 @Diff
sensStaticHMatrixMult = SensStaticHMatrix.mult @3 @4 @5 @L1 @Diff

-- sensStaticHMatrixPlus2 = (withKnownNat2 SensStaticHMatrix.plus 2 5)

$( sensCheck
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
  quickCheck $ withMaxSuccess 100 (\case SameSizedSLists trainingRows1 trainingRows2 -> clippedGradProp trainingRows1 trainingRows2 net0)

$( singleton <$> genProp 'SensStaticHMatrix.plus)

-- testStaticPlus = do
--   (x, y) <- generate $ (,) <$> choose (1, 10) <*> choose (1, 10)
--   box1 <- generate $ SensStaticHMatrix.genMatrix @L2 @Diff x y
--   box2 <- generate $ SensStaticHMatrix.genMatrix @L2 @Diff x y
--   case (box1, box2) of ((SensStaticHMatrix.Box m1), (SensStaticHMatrix.Box m2)) -> putStrLn $ show $ plusProp m1 m2 m1 m2

-- Maybe I need to use 1 existential type?
-- testStaticPlus2 = do
--   (x, y) <- generate $ (,) <$> choose (1, 10) <*> choose (1, 10)
--   box <- generate $ SensStaticHMatrix.genMatrix2 @L2 @Diff x y
--   case box of SensStaticHMatrix.Box2 m1 m2 -> putStrLn $ show $ plusProp m1 m2 m1 m2


-- testStaticPlus = do
--   SomeNat @x _ <- arbitraryKnownNat
--   SomeNat @y _ <- arbitraryKnownNat
--   m1 <- SensStaticHMatrix.exampleThree @x @y @L2 @Diff
--   m2 <- SensStaticHMatrix.exampleThree @x @y @L2 @Diff
--  quickCheck $ withMaxSuccess 100 $ plusProp m1 m2 m1 m2 

testStaticPlus =
  quickCheck
    (forAll
      (SensStaticHMatrix.genTwo
         (\m1 m2 m3 m4 -> pure (plusProp m1 m2 m3 m4))
      )
      id
    )

-- $( sensCheck
--     "failing_tests"
--     [ 'Incorrect.add_pair_solo1
--     , 'add_dependently_typed_matrix_incorrect
--     , 'Incorrect.solo_double1
--     ]
--  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pass"] -> pass
    -- ["fail"] -> fail
    otherwise -> do
      putStrLn "Defaulting to running all tests."
      putStrLn "To run specific suite run as stack test --test-arguments=\"pass|fail\""
      pass
      -- fail
  where
    pass = do
      putStrLn "\n\nThese tests are expected to pass:"
      passing_tests
      sensCheckDPClippedGrad
    -- fail = do
    --   putStrLn "These tests are expected to fail:\n\n"
    --   failing_tests

