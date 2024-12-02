{-# LANGUAGE
    DataKinds
   ,FlexibleContexts
   ,GADTs
   ,GeneralizedNewtypeDeriving
   ,KindSignatures
   ,PartialTypeSignatures
   ,PolyKinds
   ,RankNTypes
   ,ScopedTypeVariables
   ,TypeApplications
   ,TypeFamilies
   ,TypeOperators
   ,TypeSynonymInstances
   ,TypeFamilyDependencies
   ,RebindableSyntax
   ,EmptyCase
   ,MultiParamTypeClasses
   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module SFunction where

import Test.QuickCheck (Gen, quickCheck)
import Sensitivity
import qualified GHC.TypeLits as TL
import Data.Proxy
import StdLib hiding (Curry, Pair, SFunction)
import GHC.Base (Type)
import Prelude
import qualified Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Monadic (monadic)
import Data.Fixed (mod')

-- | This emulates a higher order function similar to what QuickCheck.Function ultimately produces
-- p is a proxy that may help with type unification e.g. in the case of the ScaleSens instances
-- If p is not needed then p
class SFunction a inSens b outputSens p where
    sfunctionTable :: Proxy p -> a inSens -> b outputSens

    -- We want to test against any random function rather than a static function
    -- This creates a function in the Gen monad
    -- Note: We compare against the same function. The function should be deterministic not random itself.
    -- This would be ideal however I have issues with resolving type variables in the use-site
    sfunctionTable2 :: Proxy p -> Gen (a inSens -> b outputSens)

    -- This version takes a random value to generate "random function" instead so we don't use the Gen monad
    sfunctionTable3 :: Proxy p -> Double -> a inSens -> b outputSens

-- A function that takes a SDouble and scales it *up to* a factor of scalar
instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Diff) s1 (SDouble Diff) s2 scalar where
  -- This works however this doesn't really create any random function that is n-sensitive (where n = scalar)
  sfunctionTable p d =
        let scalar = TL.natVal p
        in D_UNSAFE $ unSDouble d * fromInteger scalar

  sfunctionTable2 p = do
        let scalar' :: Gen Integer = Gen.chooseInteger (0 :: Integer, TL.natVal p)
        scalar <- scalar'
        pure $ \d -> D_UNSAFE $ unSDouble d * fromInteger scalar

  sfunctionTable3 p randomValue d =
       let scalar = TL.natVal p
           scaleFactor = randomValue `mod'` fromIntegral scalar
       in D_UNSAFE $ unSDouble d * scaleFactor

instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Disc) s1 (SDouble Disc) s2 scalar where
  sfunctionTable p d =
        let scalar = fromIntegral $ TL.natVal (Proxy @scalar)
            -- TODO might want Gen Monad or just require random or somethingl @scalar Proxy
            -- scaleUpTo = chooseInteger (0, scalar)
        in D_UNSAFE $ unSDouble d * fromInteger scalar

  -- TODO make an instance for this
    -- • No instance for ‘SFunction
    --                      a0
    --                      inSens0
    --                      ((->) (SDouble Diff s2p))
    --                      (SDouble Diff (ScaleSens s1p 1 +++ ScaleSens s2p 1))
    --                      1’
    --     arising from a use of ‘sfunctionTable3’
    -- • In the first argument of ‘($)’, namely

-- instance SFunction a inSens ((->) (b s2)) (inSens +++ )

-- instance SFunction (SDouble Diff) (ScaleSens s1 1) ((->) (SDouble Diff s2)) (SDouble Diff (ScaleSens s1 1 +++ ScaleSens s2 1)) 1 where

-- Want to take a (s1 +++ s2) eventually
-- instance (s2 ~ s1 +++ s1) => SFunction (SDouble Diff) s1 (SDouble Diff) s2 where
-- sfunctionTable d = undefined

-- | This is to get Curry to work. The original attempt was:
-- Curry '(a, b) '(sa, sb) = '(a sa, b sb)
-- But we need kind Type not kind (Type, Type)
data Pair (a :: SEnv -> Type) (b :: SEnv -> Type) (s :: (SEnv, SEnv)) where
  Pair :: a sa -> b sb -> Pair a b '(sa, sb)

type family Curry (ab :: (SEnv -> Type, SEnv -> Type)) :: (SEnv, SEnv) -> Type where
  Curry '(a, b) = Pair a b

-- Curried version of function
-- Since making the recursive case is hard maybe just make the user curry the function?
-- Could make it generic to any a,b, and c later e.g.
-- instance (s3 ~ s1 +++ s2, ab ~ Curry '(a, b)) => SFunction ab '(s1, s2) c s3 where
instance (s3 ~ s1 +++ s2, ab ~ Curry '(SDouble Diff, SDouble Diff)) => SFunction ab '(s1, s2) (SDouble Diff) s3 () where
  -- TODO probably not right
  sfunctionTable p (Pair a b) = D_UNSAFE $ unSDouble a + unSDouble b



-- Test Smap instantating the types
-- testSmap :: forall (s1 :: SEnv) (s2 :: SEnv). SList L2 (SDouble Diff) s2 -> SList L2 (SDouble Diff)  (ScaleSens s2 1) -- (ScaleSens s2 (MaxNat 1 1))
-- testSmap = smap @1 (sfunctionTable @_ @_ @_ @(ScaleSens s2 1))

-- Alternative attempt
testSmap' :: forall (s1 :: SEnv) (s2 :: SEnv). SList L2 (SDouble Diff) s2 -> SList L2 (SDouble Diff)  (ScaleSens s2 1)
testSmap' = smap' @1 @(SDouble Diff) @(SDouble Diff) (sfunctionTable  @_ @s1 @_ @(ScaleSens s1 1) (Proxy @1))

-- Attempt at using let binding to specify what s2 ~ is exactly in the instance
-- Same error
-- testSmap' :: forall (s1 :: SEnv) (s2 :: SEnv). SList L2 (SDouble Diff) s2 -> SList L2 (SDouble Diff)  (ScaleSens s2 1)
-- testSmap' = let x  :: (foo ~ ScaleSens s1 1, TL.KnownNat 1) => SDouble Diff s1 -> SDouble Diff foo = sfunctionTable @_ @s1 @_ @(ScaleSens s1 1) (Proxy @(ScaleSens s1 1))
--        in
--      smap' @1 @(SDouble Diff) @(SDouble Diff) x

-- testSmap' = smapWithProxy' @1 @(SDouble Diff) @(SDouble Diff) (Proxy @1) (sfunctionTable (Proxy @(ScaleSens s1 1)) @_ @s1 @_ @(ScaleSens s1 1))

-- testHof :: SDouble Diff (s1 +++ s2)
-- testHof = sfunctionTable @(s1 +++ s2) (SDouble Diff, SDouble Diff)

-- This is not what we really want though
-- instance (SDouble b sb c sc, sc ~ sa +++ sb) => SFunction (SDouble a sa c sc) where
-- sfunctionTable a = sfunctionTable @b @sb @c @sc a

-- Maybe this works?
-- instance  (SFunction a sa b sb, SFunction b sb c sc) => SFunction a sa c sc where
--   sfunctionTable a = sfunctionTable @b @sb @c @sc (sfunctionTable @a @sa @b @sb a)
--
-- testHof2 :: (SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2))
-- testRecursiveCase =

-- Might be nice to write this more generically e.g. but not really needed
-- type family Identity (s1 :: SEnv) :: SEnv where
--   Identity s = s
--
-- instance (s2 ~ ScaleSens s1 1) => SFunction a s1 b s2 where
--   -- sfunctionTable a = sfunctionTable @a @s1 @b @s1 (cong scale_unit a)
--   -- TODO
--   sfunctionTable = id
--
-- instance (s2 ~ Identity s1) => SFunction a s1 b s2 where
--   sfunctionTable = id

-- TODO probably use a type family to make a generic thing
-- type FlipScaleSens s1 s2 = ScaleSens s2 s1
-- instance SFunction a s1 b (FlipScaleSens 2 s1) where
--   sfunctionTable = id


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
