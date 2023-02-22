{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (unless, zipWithM, (>=>))
import Data.Coerce (coerce)
import Data.List (isInfixOf, transpose)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Debug.Trace (trace)
import Distance qualified
import GHC.Num qualified
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
import Sensitivity qualified

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

-- Generates quickcheck props, tests, and a main function that runs several tests
-- Given a list of functions that will be tested against.
-- Output: main :: IO ()
genMainQuickCheck' :: ParseSensitiveType -> String -> [Name] -> Q [Dec]
genMainQuickCheck' extractSensitiveType mainName names = do
  testsAndProps <- mapM (genQuickCheck extractSensitiveType) names
  let mainName' = mkName mainName
      testNames = \case { FunD testName _ -> testName } . fst <$> testsAndProps
      doStatement = DoE Nothing $ NoBindS . VarE <$> testNames
      testAndPropsList = concatMap tuple2ToList testsAndProps
  return $ FunD mainName' [Clause [] (NormalB doStatement) []] : testAndPropsList

-- Calls genMainQuickCheck' with a default instance of ParseSensitiveType
genMainQuickCheck :: String -> [Name] -> Q [Dec]
genMainQuickCheck = genMainQuickCheck' defaultParseSensitiveTypes

-- Generates a quickcheck test for a given function
genQuickCheck :: ParseSensitiveType -> Name -> Q (Dec, Dec)
genQuickCheck extractSensitiveType functionName = do
  prop <- genProp' extractSensitiveType functionName
  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      testName = mkName $ functionNameUnqualified <> "_test" -- The quickcheck function name we are generating
      (FunD propName _) = prop
  statement <- [|quickCheck (withMaxSuccess 1000 $(pure $ VarE propName))|]
  pure (FunD testName [Clause [] (NormalB statement) []], prop)

genProp :: Name -> Q Dec
genProp = genProp' defaultParseSensitiveTypes

genProp' :: ParseSensitiveType -> Name -> Q Dec
genProp' extractSensitiveType functionName = do
  type_ <- reifyType functionName >>= resolveTypeSynonyms
  typeAsts <- either fail pure $ parseASTs type_ extractSensitiveType

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
      <$> qNewName "input1"
      <*> qNewName "input2"
      <*> qNewName "distance"

---- START Sensitive Environment Parser ----

-- Parses template haskell's AST and returns either an error or the SensitiveType
type ParseSensitiveType = Type -> Either String SensitiveType

type ParseError = String

{-
Matches TH AST to get the unparsed Sensitive Type ASTs.
This is a default instance. If a user creates a datatype that doesn't pattern match in one of these cases
Then they may implement their own SensitiveType parser.
-}
defaultParseSensitiveTypes :: ParseSensitiveType
defaultParseSensitiveTypes typ = do
  innerAST <- case typ of
    AppT _ innerAst ->
      Right innerAst
    _ -> Left $ "defaultParseSensitiveType failed to match TH AST. Consider creating a custom ParseSensitiveType. Unmatched TH AST: " <> show typ
  parseInnerSensitiveType innerAST

-- Parses sensitive type stripped of container value.
-- e.g. This will parse (s1 +++ s2) in SDouble (s1 +++ s2) but after the SDouble is stripped.
parseInnerSensitiveType :: Type -> Either ParseError SensitiveType
parseInnerSensitiveType typ = case typ of
  (VarT name) -> pure (SEnv_ name) -- base case captures SDouble s1
  AppT (AppT (ConT binaryOp) t1) t2 -> do
    -- recursive case
    binaryOp <- nameToBinaryOp binaryOp
    term1 <- parseInnerSensitiveType t1
    term2 <- parseInnerSensitiveType t2
    Right $ binaryOp term1 term2
  _ -> Left $ "parseInnerSensitiveType failed matching on: " <> show typ

-- Parses Template Haskell AST to a list of simplified ASTs
-- SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) into a list of ASTs
parseASTs :: Type -> ParseSensitiveType -> Either ParseError [SensitiveType]
parseASTs typ extractSensitiveType =
  traverse extractSensitiveType $ splitArgs (stripForall typ)

-- Remove Forall if found
stripForall :: Type -> Type
stripForall t = case t of
  ForallT _ _ t' -> t'
  t' -> t' -- else do nothing

-- Split when encountering ->
splitArgs :: Type -> [Type]
splitArgs typ = case typ of
  AppT (AppT ArrowT t1) t2 -> splitArgs t1 ++ splitArgs t2
  t -> [t]

nameToBinaryOp :: Name -> Either ParseError (SensitiveType -> SensitiveType -> SensitiveType)
nameToBinaryOp name
  | isInfixOf "+++" $ show name = pure Plus'
  | otherwise = Left $ "Unhandled binary op" <> show name

--- END Sensitive Environment Parser ---

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
      distance <-
        case M.lookup se senvToDistance of
          Nothing -> fail $ "Unable to find sensitivity environment in distance map " <> show se <> show senvToDistance
          Just [] -> fail $ "Unable to find sensitivity environment in distance map. Empty for given key: " <> show se <> " in Map: " <> show senvToDistance
          Just (GeneratedDistanceName distanceName : _) -> pure distanceName
      -- TODO this logic was weird. Why did I do it like this before?
      -- Simplifying this fixes it. I think I was removing stuff when I shouldn't have.
      -- It was removing the assosciated distance when you don't need to.
      pure (VarE distance, senvToDistance)
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
