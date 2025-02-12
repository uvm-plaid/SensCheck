{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -ddump-splices #-}
 -- Outputs to something like .stack-work/dist/x86_64-linux/ghc-9.6.6/build/SensCheck-test/SensCheck-test-tmp/test/Spec.thr.dump-splices
-- {-# OPTIONS_GHC -ddump-to-file #-}

import AnnotatedExternalLibrary (add_dependently_typed_matrix_solo, add_matrix_solo, add_pair_solo, solo_mixed_types, solo_plus, solo_plus_incorrect)
import Control.Monad
import Control.Monad.Random
import Debug.Trace qualified as Debug
import Distance
import DpMinst (SGradients (..), flattenGrads, randomMnist)
import DpMinst qualified
import GHC.TypeLits (KnownNat)
import Sensitivity
import TH (sensCheck, sensProperty)
import Test.QuickCheck (quickCheck, withMaxSuccess, generate, choose, forAll, Fun (..), applyFun, Function(..), functionMap, CoArbitrary (coarbitrary))
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix
import StdLib (smap)
import Data.Kind (Type)
import qualified GHC.TypeNats as TypeNats
import Test.QuickCheck.Function
import Primitives (eq_sym, scale_unit, cong)
import SFunction

-- $( sensProperty
--     "hof"
--     [
--       -- 'Correct.smap'SDoubleFunction
--       'Correct.sfoldrSDoubleDiffL1
--     ]
--  )

$( singleton <$> sensProperty 'Correct.smapSDoubleDiffL2)
$( singleton <$> sensProperty 'Correct.smapSDoubleDiffL2HighSens)

$( singleton <$> sensProperty 'Correct.sfoldrSDoubleDiffL1)
$( singleton <$> sensProperty 'Correct.sfoldrSDoubleDiffL2HighSens)
$( singleton <$> sensProperty 'Correct.sfoldrSDoubleDiscL2)

-- $( sensCheck
--     "wrongHof"
--     ['Correct.sfoldrSDoubleDiffL2
--     ]
--  )

$( sensCheck
    "passingTests"
    [ 'Correct.solo_double
    , 'Correct.solo_plus
    , 'Correct.add_pair_solo
    , 'Correct.solo_mixed_types
    , 'Correct.smapIdDoubles
    , 'Correct.slistAdd42
    ]
 )

-- I think each of these need to be called seperately because it's a non-homogenous list
-- $( sensCheck
--     "passingHigherOrderFunctions"
--     [ 'Correct.smapSDouble
--     -- , 'Correct.smap'SDouble
--     -- , 'Correct.smap2'SDouble
--     ]
--  )

-- $( sensCheck
--     "failingTests"
--     [ 'Incorrect.add_pair_solo1
--     , 'Incorrect.solo_double1
--     ]
--  )

-- The below uses the sensProperty function to generate the property
-- Which the sensCheck function uses but these functions have slightly different bootstraping

-- $( singleton <$> sensProperty 'DpMinst.clippedGrad)

-- sensCheckDPClippedGrad = do
--   net0 <- evalRandIO randomMnist
--   quickCheck $ withMaxSuccess 100
--     (\case SameSizedSLists trainingRows1 trainingRows2 ->
--             clippedGradProp trainingRows1 trainingRows2 net0
--     )

-- $( singleton <$> sensProperty 'SensStaticHMatrix.plus)
-- $( singleton <$> sensProperty 'SensStaticHMatrix.multIncorrect)
-- $( singleton <$> sensProperty 'SensStaticHMatrix.scalarMult2)

-- testStaticPlus =
--   quickCheck
--     (forAll
--       (SensStaticHMatrix.genFour
--          (\m1 m2 m3 m4 -> pure (plusProp m1 m2 m3 m4))
--       )
--       id
--     )

-- testStaticScalarMult =
--   quickCheck
--     (forAll
--       (SensStaticHMatrix.genTwo
--          (\m1 m2 -> pure (scalarMult2Prop m1 m2))
--       )
--       id
--     )

-- testStaticMultIncorrect =
--   quickCheck
--     (forAll
--       (SensStaticHMatrix.genFourMult
--          (\m1 m2 m3 m4 -> pure (multIncorrectProp m1 m2 m3 m4))
--       )
--       id
--     )


instance CoArbitrary (SDouble Diff s1) where
  coarbitrary = undefined -- TODO

instance Function (SDouble Diff s) where
  function = functionMap (toRational . unSDouble ) (D_UNSAFE . fromRational)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pass"] -> pass
    ["fail"] -> fail
    _ -> do
      putStrLn "Defaulting to running all tests."
      putStrLn "To run specific suite run as stack test --test-arguments=\"pass|fail\""
      -- testSmapPropTemp
      -- Correct.smapPropMain
      -- Correct.smapMain
      -- Correct.sfoldrMain
      -- hof
--       pass
--       fail
  where
    pass = do
      putStrLn "\n\nThese tests are expected to pass:"
      quickCheck (\random (SameSizedSLists l1 l2) -> smapSDoubleDiffL2Prop l1 l2 random )
      quickCheck (\random (SameSizedSLists l1 l2) -> smapSDoubleDiffL2HighSensProp l1 l2 random )
      quickCheck (\random (SameSizedSLists l1 l2) acc1 acc2 -> sfoldrSDoubleDiffL1Prop random l1 acc1 l2 acc2)
      quickCheck (\random (SameSizedSLists l1 l2) acc1 acc2 -> sfoldrSDoubleDiscL2Prop random l1 acc1 l2 acc2)
      quickCheck (\random (SameSizedSLists l1 l2) acc1 acc2 -> sfoldrSDoubleDiffL2HighSensProp random l1 acc1 l2 acc2)
      -- testStaticPlus
      -- testStaticScalarMult
      passingTests
      -- sensCheckDPClippedGrad
    fail = do
      putStrLn "\nThese tests are expected to fail:\n\n"
      -- failingTests
      -- testStaticMultIncorrect
