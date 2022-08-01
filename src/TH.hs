{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (unless, zipWithM)
import Data.Coerce (coerce)
import Data.List (isInfixOf, transpose)
import Data.Map
import qualified Data.Map as M
import Data.Traversable (for)
import Debug.Trace (trace)
import GHC.Base (Nat)
import qualified GHC.Num
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName (ModName), Name (Name), NameFlavour (NameQ), qNewName)
import Sensitivity
import qualified Sensitivity
import qualified Verifier

-- Two issues with SEnv ->
-- Parsing logic gets duplicated
-- Pattern matching on functions isn't possible
-- This is right though SEnv should always be on the outside I believe
data Term'
  = SDouble' NMetric SEnv'
  | SMatrix' CMetric (SEnv' -> Term') SEnv'
  | SList' CMetric (SEnv' -> Term') SEnv'
  | SPair' CMetric (SEnv' -> Term') (SEnv' -> Term') SEnv'
  deriving (Show)

instance Show (SEnv' -> Term') where
  show f = "SEnv' -> Term'"

data SEnv'
  = SEnv_ Name -- Terminal Sensitivity Environment
  | Plus' SEnv' SEnv' -- +++
  | ScaleSens' SEnv' Int
  | TruncateSens'
  deriving (Show)

-- Name of sensitivty environment
-- For example the name of `SDouble Diff s1` is s1
type SEnvName = String

-- TODO clean this up
genProp :: Name -> Q [Dec]
genProp functionName = do
  type_ <- reifyType functionName
  typeAsts <- parseASTs type_

  let propName = mkName $ show functionName <> "_prop" -- The quickcheck property function name we are generatinge
      inputTypeAsts = init typeAsts -- The input types of the function in AST form
      outputTypeAst = last typeAsts -- The output of the function in AST form

  -- Generate 2 input variables and a distance variable for each Input Type
  generatedNamesForInputType <- mapM genNameForInputTypes inputTypeAsts
  -- Create a distance statement for each Input Type given the input arguments and distance argument
  distanceStatements <-
    mapM
      (\(ast, input1, input2, distance) -> genDistanceStatement ast distance input1 input2)
      generatedNamesForInputType

  -- Create the distance out statement.
  -- Gather all the arguments to the first call to F and all the arguments for the second call to F
  let inputs1 = (\(_, input1, _, _) -> input1) <$> generatedNamesForInputType
      inputs2 = (\(_, _, input2, _) -> input2) <$> generatedNamesForInputType
  (distanceOutStatement, generatedDistanceOutName) <- genDistanceOutStatement outputTypeAst functionName inputs1 inputs2

  propertyStatement <- genPropertyStatement outputTypeAst
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      body = Clause [] (NormalB statements) []
  pure [FunD propName [body]]
 where
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1" <*> qNewName "input2" <*> qNewName "distance"

-- >>> runQ [d|x :: (a,b,c); x = undefined|]
-- [SigD x_3 (AppT (AppT (AppT (TupleT 3) (VarT a_0)) (VarT b_1)) (VarT c_2)),ValD (VarP x_3) (NormalB (VarE GHC.Err.undefined)) []]

-- This might be the output of SDouble s1 -> SDouble s2 -> SDouble (s1 +++ s2)
-- You might notice I'm ignoring SDouble. Not sure if we really need to capture that.
-- exampleAstPlus :: [Term']
-- exampleAstPlus =
--   [ SDouble' Diff $ SEnv_ "s1"
--   , SDouble' Diff $ SEnv_ "s2"
--   , SDouble' Diff $ Plus' (SEnv_ "s1") (SEnv_ "s2")
--   ]
--
-- exampleAstDiscardS2 :: [Term']
-- exampleAstDiscardS2 =
--   [ SDouble' Diff $ SEnv_ "s1"
--   , SDouble' Diff $ SEnv_ "s2"
--   , SDouble' Diff $ SEnv_ "s1"
--   ]

-- Parses SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
-- into a list of ASTs
-- This is going to be painful
parseASTs :: Type -> Q [Term']
parseASTs typ = traverse typeToTerm (splitArgs (stripForall typ))
 where
  -- Remove Forall if found
  stripForall t = case t of
    ForallT _ _ t' -> t'
    t' -> t' -- else do nothing
    -- Split when encountering ->
  splitArgs :: Type -> [Type]
  splitArgs typ = case typ of
    AppT (AppT ArrowT t1) t2 -> splitArgs t1 ++ splitArgs t2
    t -> [t]
  typeToSEnv :: Type -> Q SEnv'
  typeToSEnv typ = case typ of
    (VarT name) -> pure (SEnv_ name) -- base case captures SDouble s1
    AppT (AppT (ConT binaryOp) t1) t2 -> do
      binaryOp <- nameToBinaryOp binaryOp
      term1 <- typeToSEnv t1
      term2 <- typeToSEnv t2
      pure $ binaryOp term1 term2
  nameToBinaryOp :: Name -> Q (SEnv' -> SEnv' -> SEnv')
  nameToBinaryOp name
    | isInfixOf "+++" $ show name = pure Plus'
    | otherwise = fail $ "Unhandled binary op" <> show name
  typeToTerm :: Type -> Q Term'
  typeToTerm typ = case typ of
    -- TODO last thing can be expression or SEnv'. Make a parse SEnv'. This handles SDouble s1 but not SDouble (s1 +++ s2)

    AppT (AppT (ConT termName) (PromotedT metricName)) s ->
      -- TODO I think SList and SMatrix follow this shape?
      if termName == ''Sensitivity.SDouble
        then do
          nmetric <- nameToNMetric metricName
          senv <- typeToSEnv s
          pure $ SDouble' nmetric senv
        else fail $ "typeToTerm 2 unhandled case" ++ show termName
    AppT (AppT (AppT (ConT termName) (PromotedT metricName)) innerType) s ->
      -- TODO this is another special case where innerType is missing
      if termName == ''Verifier.SMatrix
        then do
          cmetric <- nameToCMetric metricName
          senv <- typeToSEnv s
          innerTerm <- typeToTerm (AppT innerType s) --TODO I can fake this by adding the application to SEnv and then removing it. Dirty tho
          pure $ SMatrix' cmetric (const innerTerm) senv --TODO wrong
        else fail $ "typeToTerm 3 unhandled case" ++ show termName
    _ -> fail $ "typeToTerm main unhandled case" <> show typ
  nameToCMetric :: Name -> Q CMetric
  nameToCMetric name
    | name == 'Sensitivity.L1 = pure L1
    | name == 'Sensitivity.L2 = pure L2
    | name == 'Sensitivity.LInf = pure LInf
    | otherwise = fail $ "Unhandled CMetric" <> show name
  nameToNMetric :: Name -> Q NMetric
  nameToNMetric name
    | name == 'Sensitivity.Diff = pure Diff
    | name == 'Sensitivity.Disc = pure Disc
    | otherwise = fail $ "Unhandled NMetric" <> show name

-- Mock value. TODO figure out how to implement this later.

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name
newtype GeneratedDistanceName = GeneratedDistanceName Name

-- Generates
-- d1 = abs $ unSDouble input1 - unSDouble input2
-- The above uses abs but it depends on type:
-- Given a SDouble uses abs
-- Given a SMatrix uses norm function
-- Given an SList uses TODO
genDistanceStatement :: Term' -> GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement ast (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = $(τ ast) $(pure $ VarE input1) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- TODO what if it's a 3 function argument or 1.
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Term' -> Name -> [GeneratedArgName] -> [GeneratedArgName] -> Q ([Dec], GeneratedDistanceName)
genDistanceOutStatement ast functionName inputs1 inputs2 = do
  distance <- qNewName "distanceOut"
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Application = applyInputsOnFunction inputs1
      function2Application = applyInputsOnFunction inputs2
  statement <- case ast of
    SDouble' Diff _ -> [d|$(pure $ VarP distance) = abs $ unSDouble $(pure function1Application) - unSDouble $(pure function2Application)|]
    SMatrix' L2 _ _ -> [d|$(pure $ VarP distance) = norm_2 $ unSDouble $(pure function1Application) - unSDouble $(pure function2Application)|]
    _ -> fail $ "Unexpected input in genDistanceStatement AST: " <> show ast
  pure
    ( statement
    , GeneratedDistanceName distance
    )

τ :: Term' -> Q Exp
τ ast = case ast of
  SDouble' Diff _ -> [|absdist|]
  SDouble' Disc _ -> [|diff|]
  SMatrix' cmetric innerType senv ->
    let appliedInnerType = innerType senv
     in case (cmetric, appliedInnerType) of
          (L2, SDouble' Diff _) -> [|l2dist|]
          (L1, SDouble' Diff _) -> [|l1dist|]
          (L2, SDouble' Disc _) -> [|l2dist . diff|]
          (L1, SDouble' Disc _) -> [|l1dist . diff|]
  SList' cmetric innerType senv ->
    let appliedInnerType = innerType senv
     in case (cmetric, appliedInnerType) of
          (L2, SDouble' Diff _) -> [|l2dist|]
          (L1, SDouble' Diff _) -> [|l1dist|]
          (L2, SDouble' Disc _) -> [|l2dist . diff|]
          (L1, SDouble' Disc _) -> [|l1dist . diff|]
  _ -> fail $ "Unexpected input in τ AST: " <> show ast

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- Note we need to add some small padding cause floating point artimatic
genPropertyStatement :: Term' -> Q Exp
genPropertyStatement ast = [e|dout <= $(computeRhs $ getSEnv ast) + 0.00000001|]
 where
  computeRhs :: SEnv' -> Q Exp
  computeRhs sexp = case sexp of
    -- if it's just a sensitivity env (e.g. 's1') then return the distance value for it (e.g. 'd1')
    SEnv_ se -> [|s|]
    Plus' se1 se2 -> [|$(computeRhs se1) + $(computeRhs se2)|]
    ScaleSens' se1 n -> [|n * $(computeRhs se1)|]
    _ -> undefined
  -- Strip sensitivity environment from Type
  -- e.g returns (s1 +++ s2) from SDouble Diff (s1 +++ s2)
  getSEnv :: Term' -> SEnv'
  getSEnv ast = case ast of
    SDouble' _ se -> se
    SMatrix' _ _ se -> se
    SList' _ _ se -> se
    SPair' _ _ _ se -> se
