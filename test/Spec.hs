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
import Test.QuickCheck (quickCheck, withMaxSuccess, generate, choose, forAll, Fun (..), applyFun, Function(..), functionMap, CoArbitrary (coarbitrary))
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix
import StdLib (smap)
import Data.Kind (Type)
import StdLib (smap')
import qualified GHC.TypeNats as TypeNats
import Test.QuickCheck.Function
import Primitives (eq_sym, scale_unit, cong)

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
$( singleton <$> sensProperty 'SensStaticHMatrix.multIncorrect)
$( singleton <$> sensProperty 'SensStaticHMatrix.scalarMult2)

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
      (SensStaticHMatrix.genTwo
         (\m1 m2 -> pure (scalarMult2Prop m1 m2))
      )
      id
    )

testStaticMultIncorrect =
  quickCheck
    (forAll
      (SensStaticHMatrix.genFourMult
         (\m1 m2 m3 m4 -> pure (multIncorrectProp m1 m2 m3 m4))
      )
      id
    )


-- Smap for reference
-- smap' :: forall fn_sens a b s2 m.
--   (forall s1. a s1 -> b (ScaleSens s1 fn_sens))
--   -> SList m a s2
--   -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))

-- TODO What does the manually written property look like?
-- Something like this?
-- smapProp :: forall fn_sens a (b :: SEnv -> Type) s1 s2 m.
--   (Distance (b (ScaleSens s2 fn_sens))) =>
--   (a s1 -> b (ScaleSens s1 fn_sens))
--   -> SList L2 (SDouble Diff) s2
--   -> SList L2 (SDouble Diff) s2
--   -> Bool
-- smapProp f xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap @fn_sens f xs) (smap @fn_sens f ys)
--   in distOut <= distIn

-- Sensitive identity function
sid :: a s -> a (ScaleSens s 1)
sid = cong (eq_sym scale_unit)

-- Property of smap on the identity function
-- idSmapProp :: forall a s2 m.
--   SList m a s2
--   -> SList m a s2
--   -> Bool
-- idSmapProp = smapProp @1 sid


-- Like isSmapProp but with a random showable function using QuickCheck.Function
-- I would expect an id like function here
-- DO I need the inputs?
-- Can I make the 1 a type variable?
-- idkProp :: forall a s m fn_s b. Fun (a s) (b (ScaleSens fn_s 1)) -> SList m a s -> SList m a s -> Bool
-- idkProp (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap @1 f xs) (smap @1 f ys)
--       -- distOut = distance (smap f xs) (smap f ys)
--   in distOut <= distIn


smapProp' :: forall fn_sens a b s2 m.
  (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
  (forall s1. Fun (a s1) (b (ScaleSens s1 fn_sens)))
  -> SList m a s2
  -> SList m a s2
  -> Bool
smapProp' f xs ys =
  let distIn = distance xs ys
      distOut =
        distance
          @(SList m b (ScaleSens s2 (MaxNat fn_sens 1)))
          (smap @fn_sens @a @b @s2 @m (applyFun f) xs)
          (smap @fn_sens @a @b @s2 @m (applyFun f) ys)
  in distOut <= distIn

-- Not rank2
smapProp'' :: forall fn_sens a b s2 m.
  (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
  Fun (a s2) (b (ScaleSens s2 fn_sens))
  -> SList m a s2
  -> SList m a s2
  -> Bool
smapProp'' f xs ys =
  let distIn = distance xs ys
      distOut =
        distance
          @(SList m b (ScaleSens s2 (MaxNat fn_sens 1)))
          (smap' @fn_sens @a @b @s2 @m (applyFun f) xs)
          (smap' @fn_sens @a @b @s2 @m (applyFun f) ys)
  in distOut <= distIn
-- testing higher order functions
-- functionCompositionProp :: Eq c => (a -> b) -> (b -> c) -> a -> Bool
-- functionCompositionProp f g x = (g . f) x == g (f x)

-- I forgot how to use this
-- See https://stackoverflow.com/questions/52980636/how-to-write-quickcheck-on-properties-of-functions
-- So we do need to make the type variables concrete when we actually test (I think)
-- Otherwise we end up with ambigous type errors. I think that makes sense to me otherwise I'd imagine this is hard.

-- This does not work escaped scope
-- testSmap :: forall (fn_sens :: TypeNats.Nat) a b (s1 :: SEnv) m. IO ()
-- testSmap =
--   quickCheck
--     (\(f :: Fun ((SDouble Diff) s1) (SDouble Diff (ScaleSens s1 fn_sens))) (l1 :: SList L2 (SDouble Diff) s1) (l2 :: SList L2 (SDouble Diff) s1) ->
--       smapProp' @fn_sens @(SDouble Diff) @(SDouble Diff) @s1 f l1 l2)

-- Non rank 2
testSmap' :: forall (fn_sens :: TypeNats.Nat) a b (s :: SEnv) m. IO ()
testSmap' =
  quickCheck
    (\(f :: Fun ((SDouble Diff) s) (SDouble Diff (ScaleSens s fn_sens))) (l1 :: SList L2 (SDouble Diff) s) (l2 :: SList L2 (SDouble Diff) s) ->
      smapProp'' @fn_sens @(SDouble Diff) @(SDouble Diff) @s f l1 l2)

instance CoArbitrary (SDouble Diff s1) where
  coarbitrary = undefined -- TODO

instance Function (SDouble Diff s) where
  function = functionMap (toRational . unSDouble ) (D_UNSAFE . fromRational)

smapId :: SList m b s2 -> SList m b s2
smapId = cong (eq_sym scale_unit) $ smap @1 sid

slistAddConst :: SList m (SDouble Diff) s -> SList m (SDouble Diff) _
slistAddConst = smap @1 (\x -> cong scale_unit $  D_UNSAFE $ unSDouble x + 1)

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
      testStaticScalarMult
      passingTests
      sensCheckDPClippedGrad
    fail = do
      putStrLn "\nThese tests are expected to fail:\n\n"
      failingTests
      testStaticMultIncorrect
