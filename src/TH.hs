{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Control.Monad (unless, zipWithM)
import Data.Coerce (coerce)
import Data.List (isInfixOf, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Debug.Trace (trace)
import qualified Distance
import qualified GHC.Num
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (resolveTypeSynonyms)
import Language.Haskell.TH.Syntax (ModName (ModName), Name (Name), NameFlavour (NameQ), qNewName)
import Sensitivity (
  CMetric (..),
  NMetric (..),
  SDouble (unSDouble),
  SList,
  SMatrix,
 )
import qualified Sensitivity

instance Show (SensitiveType -> SensitiveType) where
  show f = "SensitiveType -> SensitiveType"

data SensitiveType
  = SEnv_ Name -- Terminal Sensitivity Environment
  | Plus' SensitiveType SensitiveType -- +++
  | ScaleSens' SensitiveType Int
  | TruncateSens'
  deriving (Show, Eq, Ord)

-- Name of sensitivty environment
-- For example the name of `SDouble Diff s1` is s1
type SEnvName = String

-- Given an SEnv return the associated distance statement
type SEnvToDistance = Map SensitiveType [GeneratedDistanceName]

-- Generates a quick check main function given tests
-- Output: main :: IO ()
genMainQuickCheck :: String -> [Name] -> Q [Dec]
genMainQuickCheck mainName names = do
  testsAndProps <- sequence $ genQuickCheck <$> names
  let mainName' = mkName mainName
      testNames = \case { FunD testName _ -> testName } . fst <$> testsAndProps
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
      senvToDistance = M.fromListWith (++) $ (\(term, _, _, distanceName) -> (term, [distanceName])) <$> generatedNamesForInputType

      -- Create the distance out statement.
      -- Gather all the arguments to the first call to F and all the arguments for the second call to F
      inputs1 = (\(_, input1, _, _) -> input1) <$> generatedNamesForInputType
      inputs2 = (\(_, _, input2, _) -> input2) <$> generatedNamesForInputType

  distanceOutStatement <- genDistanceOutStatement outputTypeAst functionName inputs1 inputs2

  propertyStatement <- genPropertyStatement outputTypeAst senvToDistance
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      inputs = (\(GeneratedArgName n) -> VarP n) <$> (inputs1 <> inputs2)
      body = Clause inputs (NormalB statements) []
  pure $ FunD propName [body]
 where
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1" <*> qNewName "input2" <*> qNewName "distance"

-- Parses Template Haskell AST to a list of simplified ASTs
-- SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) into a list of ASTs
parseASTs :: Type -> Q [SensitiveType]
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
  typeToSEnv :: Type -> Q SensitiveType
  typeToSEnv typ = case typ of
    (VarT name) -> pure (SEnv_ name) -- base case captures SDouble s1
    AppT (AppT (ConT binaryOp) t1) t2 -> do
      binaryOp <- nameToBinaryOp binaryOp
      term1 <- typeToSEnv t1
      term2 <- typeToSEnv t2
      pure $ binaryOp term1 term2
    _ -> fail $ "typeToSEnv unhandled case " <> show typ
  nameToBinaryOp :: Name -> Q (SensitiveType -> SensitiveType -> SensitiveType)
  nameToBinaryOp name
    | isInfixOf "+++" $ show name = pure Plus'
    | otherwise = fail $ "Unhandled binary op" <> show name
  typeToTerm :: Type -> Q SensitiveType
  typeToTerm typ = case typ of
    AppT (AppT (ConT termName) (PromotedT metricName)) s ->
      if termName == ''Sensitivity.SDouble
        then typeToSEnv s
        else fail $ "typeToTerm 2 unhandled case" ++ show termName
    AppT (AppT (AppT (ConT termName) (PromotedT metricName)) innerType) s ->
      -- Container like type
      -- TODO This was the case where we handled List and Matrix
      -- It might be that a user might define something that doesn't have this shape
      -- I think we can potentially make this parsing use a typeclass with a default instance
      typeToSEnv s
    _ -> fail $ "typeToTerm main unhandled case" <> show typ

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name deriving (Show, Eq, Ord)
newtype GeneratedDistanceName = GeneratedDistanceName Name deriving (Show, Eq, Ord)

-- Generates distance statement
-- e.g. d1 = abs $ unSDouble input1 - unSDouble input2
genDistanceStatement :: SensitiveType -> GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement ast (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = Distance.distance $(pure $ VarE input1) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: SensitiveType -> Name -> [GeneratedArgName] -> [GeneratedArgName] -> Q [Dec]
genDistanceOutStatement ast functionName inputs1 inputs2 =
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Applied = applyInputsOnFunction inputs1
      function2Applied = applyInputsOnFunction inputs2
   in [d|dout = Distance.distance $(pure function1Applied) $(pure function2Applied)|]

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- Note we need to add some small padding cause floating point artimatic
genPropertyStatement :: SensitiveType -> SEnvToDistance -> Q Exp
genPropertyStatement ast senvToDistance = [e|dout <= $(fst <$> computeRhs ast senvToDistance) + 0.00000001|]
 where
  computeRhs :: SensitiveType -> SEnvToDistance -> Q (Exp, SEnvToDistance)
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

-- Pair of tuple of same type to a list of 2 elements.
tuple2ToList :: (a, a) -> [a]
tuple2ToList (a, b) = [a, b]
