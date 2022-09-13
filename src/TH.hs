{-# LANGUAGE BlockArguments #-}
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
  deriving (Show, Eq, Ord)

-- Name of sensitivty environment
-- For example the name of `SDouble Diff s1` is s1
type SEnvName = String

-- Given an SEnv return the associated distance statement
-- TODO We're not handling associations to complicated s values and distance names such as s1 +++ s2.
-- I guess we could it's a weird case anyway. But allowing the representation and would rather fail late instead of early.
type SEnvToDistance = Map SEnv' [GeneratedDistanceName]

genQuickCheck :: Name -> Q [Dec]
genQuickCheck functionName = do
  prop <- genProp functionName
  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      testName = mkName $ functionNameUnqualified <> "_test" -- The quickcheck function name we are generating
      (FunD propName _) = prop
  statement <- [|quickCheck (withMaxSuccess 1000 $(pure $ VarE propName))|]
  pure [FunD propName [Clause [] (NormalB statement) []]]

genProp :: Name -> Q Dec
genProp functionName = do
  type_ <- reifyType functionName
  typeAsts <- parseASTs type_

  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      propName = mkName $ functionNameUnqualified <> "_prop" -- The quickcheck property function name we are generating
      inputTypeAsts = init typeAsts -- The input types of the function in AST form
      outputTypeAst = last typeAsts -- The output of the function in AST form

  -- Generate 2 input variables and a distance variable for each Input Type
  generatedNamesForInputType <- mapM genNameForInputTypes inputTypeAsts
  -- Create a distance statement for each Input Type given the input arguments and distance argument
  distanceStatements <-
    mapM
      (\(ast, input1, input2, distance) -> genDistanceStatement ast distance input1 input2)
      generatedNamesForInputType

  let senvToDistance :: SEnvToDistance
      senvToDistance = fromListWith (++) $ (\(term, _, _, distanceName) -> (getSEnv term, [distanceName])) <$> generatedNamesForInputType

      -- Create the distance out statement.
      -- Gather all the arguments to the first call to F and all the arguments for the second call to F
      inputs1 = (\(_, input1, _, _) -> input1) <$> generatedNamesForInputType
      inputs2 = (\(_, _, input2, _) -> input2) <$> generatedNamesForInputType
  distanceOutStatement <- genDistanceOutStatement outputTypeAst functionName inputs1 inputs2

  propertyStatement <- genPropertyStatement outputTypeAst senvToDistance
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      inputs = (\(GeneratedArgName n) -> VarP n) <$> (inputs1 <> inputs2) -- TODO I think I need to actually interleave these
      body = Clause inputs (NormalB statements) []
  pure $ FunD propName [body]
 where
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1" <*> qNewName "input2" <*> qNewName "distance"

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
    _ -> fail $ "typeToSEnv unhandled case " <> show typ
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
      if termName == ''Sensitivity.SMatrix || termName == ''Sensitivity.SList
        then do
          cmetric <- nameToCMetric metricName
          senv <- typeToSEnv s
          -- innerType should be turned into an SEnv -> Term. This applies the outer s environment to use parser logic.
          let innerTypeApplied = AppT innerType s
          innerTerm <- typeToTerm innerTypeApplied
          pure $ SMatrix' cmetric (stripSEnvApp innerTerm) senv
        else fail $ "typeToTerm 3 unhandled case " ++ show termName
    _ -> fail $ "typeToTerm main unhandled case" <> show typ
  -- This removes a senvironment temporary added to use same parsing logic that should be stripped again
  stripSEnvApp :: Term' -> (SEnv' -> Term')
  stripSEnvApp typ = case typ of
    SDouble' c _ -> SDouble' c
    SMatrix' c i _ -> SMatrix' c i
    SList' c i _ -> SList' c i
    SPair' c i1 i2 _ -> SPair' c i1 i2
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

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name deriving (Show, Eq, Ord)
newtype GeneratedDistanceName = GeneratedDistanceName Name deriving (Show, Eq, Ord)

-- Generates distance statement
-- e.g. d1 = abs $ unSDouble input1 - unSDouble input2
genDistanceStatement :: Term' -> GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement ast (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = $(τ ast) $ $(unwrap ast) $(pure $ VarE input1) - $(unwrap ast) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Term' -> Name -> [GeneratedArgName] -> [GeneratedArgName] -> Q [Dec]
genDistanceOutStatement ast functionName inputs1 inputs2 =
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Application = applyInputsOnFunction inputs1
      function2Application = applyInputsOnFunction inputs2
   in [d|dout = $(τ ast) $ $(unwrap ast) $(pure function1Application) - $(unwrap ast) $(pure function2Application)|]

unwrap :: Term' -> Q Exp
unwrap ast = case ast of
  SDouble' _ _ -> [|unSDouble|]
  SMatrix' _ _ _ -> [|toDoubleMatrix|]
  _ -> fail $ "Unhandled input in unwrap AST: " <> show ast

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
-- TODO this doesn't handle distances associated to complex sensitivities such as s1 +++ s2.
-- That is an unusual thing for someone to do but I think we could solve it by checking the map at every case for the given expression.
-- talk to Joe Near about it
genPropertyStatement :: Term' -> SEnvToDistance -> Q Exp
genPropertyStatement ast senvToDistance = [e|dout <= $(fst <$> computeRhs (getSEnv ast) senvToDistance) + 0.00000001|]
 where
  computeRhs :: SEnv' -> SEnvToDistance -> Q (Exp, SEnvToDistance)
  computeRhs sexp senvToDistance = case sexp of
    -- if it's just a sensitivity env (e.g. 's1') then return the distance value for it (e.g. 'd1')
    se@(SEnv_ sname) -> do
      distance <- case M.lookup se senvToDistance of
        Nothing -> fail $ "Unable to find sensitivity environment in distance map " <> show se <> show senvToDistance
        Just [] -> fail $ "Unable to find sensitivity environment in distance map. Empty for given key. " <> show se <> show senvToDistance
        Just (GeneratedDistanceName distanceName : _) -> pure distanceName
      let newSEnvToDistance = M.update (\(_ : distances) -> Just distances) se senvToDistance
       in pure (VarE distance, newSEnvToDistance)
    Plus' se1 se2 -> do
      (nextLeft, leftSenvToDistance) <- computeRhs se1 senvToDistance
      (nextRight, rightSenvToDistance) <- computeRhs se2 leftSenvToDistance
      nextExp <- [|$(pure nextLeft) + $(pure nextRight)|]
      pure (nextExp, rightSenvToDistance)
    ScaleSens' se1 n -> do
      (nextInnerExp, nextSenvToDistance) <- computeRhs se1 senvToDistance
      nextExp <- [|n * $(pure nextInnerExp)|]
      pure (nextExp, nextSenvToDistance)
    _ -> undefined

-- Strip sensitivity environment from Type
-- e.g returns (s1 +++ s2) from SDouble Diff (s1 +++ s2)
getSEnv :: Term' -> SEnv'
getSEnv ast = case ast of
  SDouble' _ se -> se
  SMatrix' _ _ se -> se
  SList' _ _ se -> se
  SPair' _ _ _ se -> se
