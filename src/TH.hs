{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Control.Monad (unless, zipWithM)
import Data.Coerce (coerce)
import Data.List (isInfixOf, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (for)
import Debug.Trace (trace)
import GHC.Base (Nat)
import qualified GHC.Num
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName (ModName), Name (Name), NameFlavour (NameQ), qNewName)
import Sensitivity
    ( SMatrix, SList, CMetric(..), SDouble(unSDouble), NMetric(..) )
import qualified Sensitivity
import Language.Haskell.TH.Datatype (resolveTypeSynonyms)
import qualified TestTypeclass
import Data.Proxy (Proxy (..))

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
type SEnvToDistance = Map SEnv' [GeneratedDistanceName]

-- Generates a quick check main function given tests
-- Output: main :: IO ()
genMainQuickCheck :: String -> [Name] -> Q [Dec]
genMainQuickCheck mainName names = do
  testsAndProps <- sequence $ genQuickCheck <$> names
  let mainName' = mkName mainName
      testNames = \case {FunD testName _ -> testName} . fst <$> testsAndProps
      doStatement = DoE Nothing $ NoBindS . VarE <$> testNames
      testAndPropsList = concat $ tuple2ToList <$> testsAndProps
  return $ FunD mainName' [Clause [] (NormalB doStatement) []] : testAndPropsList

-- Generates a quickcheck test for a given function
genQuickCheck :: Name -> Q (Dec, Dec)
genQuickCheck functionName = do
  prop <- genProp functionName
  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      testName = mkName $ functionNameUnqualified <> "_test" -- The quickcheck function name we are generating
      (FunD propName _) = prop
  statement <- [|quickCheck (withMaxSuccess 1000 $(pure $ VarE propName))|]
  pure (FunD testName [Clause [] (NormalB statement) []], prop)


genProp :: Name -> Q Dec
genProp functionName = do
  type_ <- reifyType functionName >>= resolveTypeSynonyms
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
      senvToDistance = M.fromListWith (++) $ (\(term, _, _, distanceName) -> (getSEnv term, [distanceName])) <$> generatedNamesForInputType

      -- Create the distance out statement.
      -- Gather all the arguments to the first call to F and all the arguments for the second call to F
      inputs1 = (\(_, input1, _, _) -> input1) <$> generatedNamesForInputType
      inputs2 = (\(_, _, input2, _) -> input2) <$> generatedNamesForInputType
      
  distanceOutStatement <- genDistanceOutStatement  outputTypeAst functionName inputs1 inputs2

  propertyStatement <- genPropertyStatement outputTypeAst senvToDistance
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      inputs = (\(GeneratedArgName n) -> VarP n) <$> (inputs1 <> inputs2) -- TODO I think I need to actually interleave these
      body = Clause inputs (NormalB statements) []
  pure $ FunD propName [body]
 where
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1" <*> qNewName "input2" <*> qNewName "distance"

-- TODO maybe I can provide a simplified AST to generate instead of the user supplying TH? Optional
-- TODO I don't see how I can actually call generate at runtime
-- This might be super hacky but maybe I can call reifyInstances call runQ on that function
-- class GenQCProp a where
--   generate :: proxy a -> Q Exp

-- instance GenQCProp (SMatrix a b c) where
--   generate _ = undefined

-- TODO try Joe's thing without a proxy and see if it works

class Distance a where
  distance :: a -> a -> Double

listType = ''[]

-- TODO try a list ''[]

intType = ''Integer

test = testHelper intType

testHelper :: Name -> Q ()
testHelper typeName = do -- typeName = SMatrix L2 ...
  let willusethislater = $(litE $ StringL $ TestTypeclass.tcmember (Proxy @$typeName)) 
  trace ("willusethislater: " <> show willusethislater) $ pure ()
  pure undefined

-- TODO this might be helpful but maybe not really tbh
-- [InstanceD _ _ (AppT _ (ConT typename)) []] <- reifyInstances className [ConT typeName] -- TODO handle failure case

-- newtype SList (m :: CMetric) (f :: SEnv -> *) (s :: SEnv) = SList_UNSAFE { unSList :: [f s] }
instance GenQCProp (SList cmetric innerType senv) where
  generate = undefined

-- Parses Template Haskell AST to a list of simplified ASTs
-- SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) into a list of ASTs
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
    AppT (AppT (ConT termName) (PromotedT metricName)) s ->
      if termName == ''Sensitivity.SDouble
        then do
          nmetric <- nameToNMetric metricName
          senv <- typeToSEnv s
          pure $ SDouble' nmetric senv
        else fail $ "typeToTerm 2 unhandled case" ++ show termName
    AppT (AppT (AppT (ConT termName) (PromotedT metricName)) innerType) s ->
      -- This is a special case where innerType is missing
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
  [d|$(pure $ VarP distance) = $(genCalcDistance ast (VarE input1) (VarE input2))|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement ::  Term' -> Name -> [GeneratedArgName] -> [GeneratedArgName] -> Q [Dec]
genDistanceOutStatement ast functionName inputs1 inputs2 =
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Application = applyInputsOnFunction inputs1
      function2Application = applyInputsOnFunction inputs2
   in [d|dout = $(genCalcDistance ast function1Application function2Application)|]


genCalcDistance :: Term' -> Exp -> Exp -> Q Exp
genCalcDistance ast exp1 exp2 = case ast of
  SDouble' Diff _ -> [e|absdist $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2)|]
  SDouble' Disc _ -> [e|diff $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2)|]
  SMatrix' cmetric innerType senv -> genInnerType cmetric innerType senv
  SList' cmetric innerType senv -> genInnerType cmetric innerType senv
  _ -> fail $ "Unexpected input in τ AST: " <> show ast
  where
    genInnerType :: CMetric -> (SEnv' -> Term') -> SEnv' -> Q Exp
    genInnerType cmetric innerType senv = 
      let appliedInnerType = innerType senv
      in case (cmetric, appliedInnerType) of
            (L2, SDouble' Diff _) -> [|l2dist $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2) |]
            (L1, SDouble' Diff _) -> [|l1dist $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2) |]
            (L2, SDouble' Disc _) -> [|l2dist . diff $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2) |]
            (L1, SDouble' Disc _) -> [|l1dist . diff $ $(unwrap ast) $(pure exp1) - $(unwrap ast) $(pure exp2) |]
    unwrap :: Term' -> Q Exp
    unwrap ast = case ast of
      SDouble' _ _ -> [e|unSDouble|]
      SMatrix' _ _ _ -> [e|toDoubleMatrix|]
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

-- Pair of tuple of same type to a list of 2 elements.
tuple2ToList :: (a, a) -> [a]
tuple2ToList (a, b) = [a, b]
