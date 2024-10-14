{-# LANGUAGE
    AllowAmbiguousTypes
   ,DataKinds
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
   ,UndecidableInstances
   ,RebindableSyntax
   ,EmptyCase
  ,MultiParamTypeClasses
   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module StdLib where
import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity
import Privacy
import Primitives
import Data.Kind (Type)
import Test.QuickCheck (chooseInt, chooseInteger)
import Data.Tuple.Singletons (Snd, Fst)


summationFunction :: L1List (SDouble Disc) senv -> SDouble Diff (TruncateSens 1 senv)
summationFunction xs = sum (clipL1 xs)

sumFn :: forall s1 s2. SDouble Diff s1 -> SDouble Diff s2 -> SDouble Diff (ScaleSens s1 1 +++ ScaleSens s2 1)
sumFn x y = (cong (eq_sym scale_unit) x) <+> (cong (eq_sym scale_unit) y)

sum :: forall s. L1List (SDouble Diff) s -> SDouble Diff s
sum xs = cong scale_unit $ sfoldr @1 @1 sumFn (sConstD @'[] 0) xs

clipList :: L1List (SDouble Disc) s -> L1List (SDouble Diff) s
clipList xs = cong scale_unit $ smap @1 (\x -> cong (eq_sym scale_unit) $ clipDouble x) xs

smap :: forall fn_sens a b s2 m.
  (forall s1. a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap f as = sfoldr @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] []) as

-- Version of smap without rank 2 types
smap' :: forall fn_sens a b s2 m s1.
  (a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])

-- | This emulates a higher order function similar to what QuickCheck.Function ultimately produces
-- SensCheck call this on higher order functions
-- TODO how would I support this for 3 or more arguments?
class SFunction a inSens b outputSens where
    sfunctionTable :: a inSens -> b outputSens

-- A function that takes a SDouble and scales it *up to* a factor of scalar
instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Diff) s1 (SDouble Diff) s2 where
  sfunctionTable d =
        let scalar = TL.natVal @scalar Proxy
            -- TODO might want Gen Monad or just require random or something
            -- scaleUpTo = chooseInteger (0, scalar)
        in D_UNSAFE $ unSDouble d * fromInteger scalar

instance (s2 ~ ScaleSens s1 scalar, TL.KnownNat scalar) => SFunction (SDouble Disc) s1 (SDouble Disc) s2 where
  sfunctionTable d =
        let scalar = fromIntegral $ TL.natVal (Proxy @scalar)
            -- TODO might want Gen Monad or just require random or somethingl @scalar Proxy
            -- scaleUpTo = chooseInteger (0, scalar)
        in D_UNSAFE $ unSDouble d * fromInteger scalar

-- Want to take a (s1 +++ s2) eventually
-- instance (s2 ~ s1 +++ s1) => SFunction (SDouble Diff) s1 (SDouble Diff) s2 where
-- sfunctionTable d = undefined

-- Well maybe not curry
-- type family Curry (ab :: (SEnv -> Type,  SEnv -> Type))  :: (SEnv, SEnv) -> (Type, Type) where
--   Curry '(a, b) = \'(sa, sb) -> '(a sa, b sb)

data Pair (a :: SEnv -> Type) (b :: SEnv -> Type) (s :: (SEnv, SEnv)) where
  Pair :: a sa -> b sb -> Pair a b '(sa, sb)

type family Curry (ab :: (SEnv -> Type, SEnv -> Type)) :: (SEnv, SEnv) -> Type where
  Curry '(a, b) = Pair a b
--   Curry '(a, b) '(sa, sb) = '(a sa, b sb) -- Use the Pair type to satisfy (Type, Type) does not match Type

-- Curried version of function
-- Since making the recursive case is hard maybe just make the user curry the function?
-- Could make it generic to any a,b, and c later e.g.
-- instance (s3 ~ s1 +++ s2, ab ~ Curry '(a, b)) => SFunction ab '(s1, s2) c s3 where
instance (s3 ~ s1 +++ s2, ab ~ Curry '(SDouble Diff, SDouble Diff)) => SFunction ab '(s1, s2) (SDouble Diff) s3 where
  -- TODO do something else besides just add them?
  sfunctionTable (Pair a b) = D_UNSAFE $ unSDouble a + unSDouble b


-- Test Smap instantating the types
testSmap :: forall (s1 :: SEnv) (s2 :: SEnv). SList L2 (SDouble Diff) s2 -> SList L2 (SDouble Diff)  (ScaleSens s2 1) -- (ScaleSens s2 (MaxNat 1 1))
testSmap = smap @1 (sfunctionTable @_ @_ @_ @(ScaleSens s2 1))

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


sfilter :: (forall s. Double -> Bool)
  -> L1List (SDouble m) s1
  -> L1List (SDouble m) s1
sfilter f xs = cong scale_unit $ sfoldr @1 @1 (sfilter_fn f) (sConstL @'[] []) xs

count :: SList cm t s -> SDouble Diff s
count xs = cong scale_unit $ sfoldr @1 @1 count_fn (sConstD @'[] 0) xs
  where count_fn :: forall (s1 :: SEnv) s2 t.
          t s1 -> SDouble Diff s2 -> SDouble Diff (ScaleSens s1 1 +++ ScaleSens s2 1)
        count_fn x a = (cong (eq_sym scale_unit) $ sConstD @s1 1) <+> (cong (eq_sym scale_unit) a)



-- add the elements of a pair
pairPlus :: L1Pair (SDouble Diff) (SDouble Diff) s -> SDouble Diff (ScaleSens s 1)
pairPlus p = p_elim @1 @1 (\x y -> (cong (eq_sym scale_unit) x) <+> (cong (eq_sym scale_unit) y)) p

sListSum :: forall s1 s2 m. SList m (SDouble Diff) s1 -> SList m (SDouble Diff) s2
  -> SList m (SDouble Diff) (ScaleSens s1 1 +++ ScaleSens s2 1)
sListSum xs ys = cong (scale_distrib @1 @s1 @s2) $ smap @1 pairPlus $ szip xs ys

sListSum1s :: forall s1 s2 m. SList m (SDouble Diff) s1 -> SList m (SDouble Diff) s2
  -> SList m (SDouble Diff) (s1 +++ s2)
sListSum1s xs ys = cong scale_unit $ smap @1 pairPlus $ szip xs ys

sfoldr1s :: forall t1 t2 cm s3 s4 s5.
           (forall s1 s2. t1 s1 -> t2 s2 -> t2 (s1 +++ s2))
        -> t2 s5 -> SList cm t1 s4 -> t2 (s4 +++ TruncateInf s5)
sfoldr1s f init xs = cong prf $ sfoldr @1 @1 f' init xs
  where f' :: t1 s1 -> t2 s2 -> t2 (ScaleSens s1 1 +++ ScaleSens s2 1)
        f' a b = f (cong (eq_sym scale_unit) a) (cong (eq_sym scale_unit) b)
        prf = plus_cong @(ScaleSens s4 1) @s4 @(TruncateInf s5) scale_unit Id

