{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}

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
import Test.QuickCheck (quickCheck, withMaxSuccess, generate, choose, forAll, Fun (..))
import Utils
import Data.List (singleton)
import System.Environment (getArgs)

import qualified AnnotatedExternalLibrary as Correct
import qualified IncorrectAnnotations as Incorrect
import qualified SensStaticHMatrix as SensStaticHMatrix
import StdLib (smap)
import Data.Kind (Type)
import StdLib (smap')

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
-- sid :: a s -> a (ScaleSens s 1)
-- sid x = undefined

-- Property of smap on the identity function
-- idSmapProp :: forall a s2 m.
--   SList m a s2
--   -> SList m a s2
--   -> Bool
-- idSmapProp = smapProp @1 sid


-- TODO reimplmenet using this: 
-- >>> let prop :: Fun String Integer -> Bool
-- >>> prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"

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


-- Not working maybe the rank 2 types are the issue?
-- smapProp' :: 
--   -- (forall s1. a s1 -> b (ScaleSens s1 fn_sens))
--   (Fun (a s1) (b (ScaleSens s1 fn_sens)))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap f xs) (smap f ys)
--   in distOut <= distIn


-- Confusing constraints required and fn_sens doesn't match
-- smapProp'' :: forall fn_sens a b s2 m.
--   (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
--   (forall s1. Fun (a s1) (b (ScaleSens s1 fn_sens)))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp'' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap f xs) (smap f ys)
--   in distOut <= distIn -- TODO scale this


-- This complains about s1 excaping scope
-- smapProp''' :: forall fn_sens a b s2 m x f.
--   (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
--   (forall s1. Fun (a s1) (b (ScaleSens s1 fn_sens)))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp''' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance @(SList m b (ScaleSens s2 (MaxNat fn_sens 1))) (smap @fn_sens @a @b @s2 @m (\x -> f x) xs) (smap @fn_sens @a @b @s2 @m (\x -> f x) ys)
--   in distOut <= distIn

-- What if I just move the s1 up?
-- smapProp'''' :: forall fn_sens a b s2 m s1.
--   (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
--   (Fun (a s1) (b (ScaleSens s1 fn_sens)))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp'''' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance @(SList m b (ScaleSens s2 (MaxNat fn_sens 1))) (smap @fn_sens @a @b @s2 @m (f :: a s1 -> b (ScaleSens s1 fn_sens) ) xs) (smap @fn_sens @a @b @s2 @m (f :: a s1 -> b (ScaleSens s1 fn_sens)) ys)
--   in distOut <= distIn

-- Joe's suggestion Remove forall
-- smapProp''' ::
--   (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
--   Fun (a s1) (b (ScaleSens s1 fn_sens))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp''' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap f xs) (smap f ys)
--   in distOut <= distIn


-- Joe's other suggestion
-- smapProp''' ::
--   (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
--   (forall s1. Fun (a s1) (b (ScaleSens s1 fn_sens)))
--   -> SList m a s2
--   -> SList m a s2
--   -> Bool
-- smapProp''' (Fun _ f) xs ys =
--   let distIn = distance xs ys
--       distOut = distance (smap f xs) (smap f ys)
--   in distOut <= distIn


-- Alternative version of smap that doesn't have the forall
smapProp''' :: forall fn_sens a b s2 m x f s1.
  (Distance (SList m a s2), Distance (SList m b (ScaleSens s2 (MaxNat fn_sens 1)))) =>
  Fun (a s1) (b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m a s2
  -> Bool
smapProp''' (Fun _ f) xs ys =
  let distIn = distance xs ys
      distOut = distance @(SList m b (ScaleSens s2 (MaxNat fn_sens 1))) (smap' @fn_sens @a @b @s2 @m f xs) (smap' @fn_sens @a @b @s2 @m f ys)
  in distOut <= distIn

-- testing higher order functions
-- functionCompositionProp :: Eq c => (a -> b) -> (b -> c) -> a -> Bool
-- functionCompositionProp f g x = (g . f) x == g (f x)

-- testFunctionComposition =
--   quickCheck (forAll functionCompositionProp)

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
