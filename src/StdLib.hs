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
   #-}
module StdLib where
import Prelude hiding (return,(>>=), sum)
import qualified Prelude as P
import qualified GHC.TypeLits as TL
import Data.Proxy

import Sensitivity
import Privacy
import Primitives
import qualified GHC.Generics


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
-- smap' :: forall fn_sens a b s2 m.
--   (a s2 -> b (ScaleSens s2 fn_sens))
--   -> SList m a s2
--   -> SList m b (ScaleSens s2 (MaxNat fn_sens 1))
-- smap' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])
-- Run this as is. Test it. Then maybe use cong later to add MaxNat.

--
-- smap' :: (forall s1. f1 s1 -> f2 (ScaleSens s1 fn_sens)) -> SList m1 f1 s2 -> SList m2 f2 (ScaleSens s2 (MaxNat fn_sens 1))
 smap' f (SList_UNSAFE l) = cong (eq_sym scale_unit) $ SList_UNSAFE $ map f l

-- Do the above after figureing this out. Which is rank2
-- smap' :: (f1 s -> f2 a) -> SList m1 f1 s -> SList m2 f2 (ScaleSens a 1)
 -- smap' :: (f1 s1 -> f2 (ScaleSens s1 fn_sens)) -> SList m1 f1 s2 -> SList m2 f2 (ScaleSens s2 (MaxNat fn_sens 1))
smap' :: (f1 s1 -> f2 s1) -> SList m1 f1 s1 -> SList m2 f2 s1
smap' f (SList_UNSAFE l) = SList_UNSAFE $ map f l

-- Version of smap without rank 2 types AND fixed to SDouble.
-- I think the above doesn't work because I need to unwrap and rewrap?
smap'' :: forall fn_sens s2 m c.
  (SDouble c s2 -> SDouble c (ScaleSens s2 fn_sens))
  -> SList m (SDouble c) s2
  -> SList m (SDouble c) (ScaleSens s2 (MaxNat fn_sens 1))
-- smap' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])
smap'' f (SList_UNSAFE l) = SList_UNSAFE @_ @_ @(ScaleSens s2 (MaxNat fn_sens 1)) $ fmap (D_UNSAFE . unSDouble . f) l

-- Version of smap without rank 2 types and fixed to SDouble.
-- I think the above doesn't work because I need to unwrap and rewrap?
-- smap' :: forall fn_sens a b s2 m c.
--   (SDouble c s2 -> SDouble c (ScaleSens s2 fn_sens))
--   -> SList m (SDouble c) s2
--   -> SList m (SDouble c) (ScaleSens s2 (MaxNat fn_sens 1))
-- -- smap' f = sfoldr' @fn_sens @1 (\x xs -> scons (f x) (cong (eq_sym scale_unit) xs)) (sConstL @'[] [])
-- smap' f (SList_UNSAFE l) = SList_UNSAFE @_ @_ @(ScaleSens s2 (MaxNat fn_sens 1)) $ fmap (D_UNSAFE . unSDouble . f) l

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

