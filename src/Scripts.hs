{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Scripts where

import Language.Haskell.TH
import AnnotatedExternalLibrary
import qualified Sensitivity
import qualified DpMinst
import Language.Haskell.TH.Datatype (resolveTypeSynonyms)
import Debug.Trace

-- A playground for a few scripts

-- Split when encountering ->
splitArgs :: Type -> [Type]
splitArgs typ = case typ of
  AppT (AppT ArrowT t1) t2 -> splitArgs t1 ++ splitArgs t2
  t -> [t]

-- Remove Forall if found
stripForall :: Type -> Type
stripForall t = case t of
  -- TODO for track KnownNat I need to get the constraint out of the forall
  ForallT _ _ t' -> t'
  t' -> t' -- else do nothing


hasSEnv name = do
    t <- reifyType name >>= resolveTypeSynonyms -- Maybe resolve type snonymns
    stringE $ case t of
      ForallT (_ : KindedTV name _ appT': _) _ t' -> traceShow t $ show $ appT appT'
      l -> "nope " <> show t
  where
    -- Search through AppT
    appT t =
      case t of
        (AppT t1 (ConT kind)) -> traceShow t1 $ show kind == "Sensitivity.SEnv" || show kind == "Sensitivity.Sensitivity" || appT t1
        -- More likely to be in t2. I'm not sure if it will ever appear in t1.
        (AppT t1 t2) -> appT t2 || appT t1
        _ -> False
 
