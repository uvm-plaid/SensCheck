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
    -- We want to test against any random function rather than a static function
    -- This creates a function in the Gen monad
    -- Note: We compare against the same function. The function should be deterministic not random itself.
    -- This would be ideal however I have issues with resolving type variables in the use-site
    -- sfunctionTable :: Proxy p -> Gen (a inSens -> b outputSens)
    -- This version takes a random value to generate "random function" instead so we don't use the Gen monad
    sfunctionTable :: Proxy p -> Double -> a inSens -> b outputSens

-- A function that takes a SDouble and scales it *up to* a factor of scalar
instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Diff) s1 (SDouble Diff) s2 scalar where
  sfunctionTable p random d =
       let scalar = TL.natVal p
           scaleFactor = random `mod'` fromIntegral scalar
       in D_UNSAFE $ unSDouble d * scaleFactor

instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Disc) s1 (SDouble Disc) s2 scalar where
  sfunctionTable p random d =
       let scalar = TL.natVal p
           scaleFactor = random `mod'` fromIntegral scalar
       in D_UNSAFE $ unSDouble d * scaleFactor

-- Two argument functions
class SFunction2 a aInSens b bInSens c outputSens p1 p2 where
    sfunctionTable2 :: Proxy p1 -> Proxy p2 -> Double -> a aInSens -> b bInSens -> c outputSens

instance (s3 ~ ScaleSens s1 scalar1 +++ ScaleSens s2 scalar2, TL.KnownNat scalar1, TL.KnownNat scalar2) => SFunction2 (SDouble m) s1 (SDouble m) s2 (SDouble m) s3 scalar1 scalar2 where
  sfunctionTable2 p1 p2 random d1 d2 =
       let scalar1 = TL.natVal p1
           scalar2 = TL.natVal p2
           scaleFactor1 = random `mod'` fromIntegral scalar1
           scaleFactor2 = random `mod'` fromIntegral scalar2
       in D_UNSAFE $ unSDouble d1 * scaleFactor1 + unSDouble d2 * scaleFactor2
