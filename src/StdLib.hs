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
   ,RebindableSyntax
   ,EmptyCase
  ,MultiParamTypeClasses
   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QualifiedDo #-}

module StdLib where

import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity
import Privacy
import Primitives
import Data.Kind (Type)
import Data.Tuple.Singletons (Snd, Fst)
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Gen as Gen

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

class SPrimitive a b where
        wrap :: forall s. a -> b s
        unwrap :: forall s. b s -> a

instance SPrimitive Double (SDouble m) where
  wrap = D_UNSAFE
  unwrap = unSDouble

-- smap without foldr
-- to do this we can unwrap and wrap SList
-- however we also need to unwrap and rewrap the type a then
-- which is unknown
-- Turns out Solo might not have made the right assumption about implementing primative higher order functions
smap_ :: forall fn_sens a b s2 m unsensa unsensb. (SPrimitive unsensa a, SPrimitive unsensb b) =>
  (forall s1. a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap_ f (SList_UNSAFE l) = SList_UNSAFE $ (wrap @unsensb @b) . unwrap . f <$> l

smap_NoRank2 :: forall fn_sens a b s2 m unsensa unsensb. (SPrimitive unsensa a, SPrimitive unsensb b) =>
  (a s2 -> b (ScaleSens s2 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))

smap_NoRank2 f (SList_UNSAFE l) = SList_UNSAFE $ (wrap @unsensb @b) . unwrap . f <$> l

sfoldr_ :: forall fn_sens1 fn_sens2 t1 t2 cm s3 s4 s5 unsenst1 unsenst2. (SPrimitive unsenst1 t1, SPrimitive unsenst2 t2) =>
           (forall s1p s2p.
            t1 s1p -> t2 s2p -> t2 ((ScaleSens s1p fn_sens1) +++ (ScaleSens s2p fn_sens2)))
        -> t2 s5 -- Initial Acc
        -> SList cm t1 s4
        -> t2 ((ScaleSens s4 (MaxNat fn_sens1 fn_sens2)) +++ TruncateInf s5)
sfoldr_ f init (SList_UNSAFE xs) = wrap @unsenst2 @t2 $ unwrap $ foldr (\x acc -> wrap @unsenst2 @t2 . unwrap $ f x acc ) init xs
-- TODO now write a test for sfoldr_ and smap_

-- So let's write a specialized version
-- This works but isn't polymorphic now :(
smapSDouble_ :: (forall (s1 :: SEnv).  SDouble Diff s1 -> SDouble Diff (ScaleSens s1 1)) -> SList m (SDouble Diff) s2 -> SList m (SDouble Diff) (ScaleSens s2 1)
smapSDouble_ f (SList_UNSAFE l) = SList_UNSAFE $ D_UNSAFE . unSDouble . f <$> l

-- Version of smap without rank 2 types
smap' :: forall fn_sens a b s2 m s1.
  (a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])

-- Or maybe I wanted this?
smap2' :: forall fn_sens a b s2 m.
  (a s2 -> b (ScaleSens s2 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap2' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])

smapWithProxy' :: forall fn_sens a b s2 m s1.
  Proxy fn_sens
  -> (a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smapWithProxy' Proxy f = undefined

smap'''' :: forall fn_sens a b s2 m s1.
  (a s1 -> b (ScaleSens s1 fn_sens))
  -> SList m a s2
  -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
smap'''' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])

-- wrong version of smap but to test hof
smap'' :: forall fn_sens a b s m.
  (a s -> b s)
  -> SList m a s
  -> SList m b s
smap'' f = undefined

smap''' :: forall fn_sens a b s m.
  (a s -> b s)
  -> SList m a s
  -> SList m b (ScaleSens s fn_sens)
smap''' f = undefined



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

