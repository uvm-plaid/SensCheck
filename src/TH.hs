{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (replicateM, unless, when, zipWithM, (>=>))
import Control.Monad.IO.Class (liftIO)
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
import Language.Haskell.TH.Ppr (pprint)
import Language.Haskell.TH.Syntax (Lift (lift), ModName (ModName), Name (Name), NameFlavour (NameQ), qNewName)
import Safe (initMay, lastMay)
import Sensitivity (
  CMetric (..),
  NMetric (..),
  SDouble (unSDouble),
  SList,
  SMatrix,
 )
import Sensitivity qualified

-- Enable verbose logging
verbose :: Bool
verbose = True

data SensitiveAST
  = SEnv_ Name -- Terminal Sensitivity Environment
  | Plus' SensitiveAST SensitiveAST -- +++
  | ScaleSens' SensitiveAST Int
  | JoinSens' SensitiveAST SensitiveAST -- Max(a,b)
  | TruncateSens'
  deriving (Show, Eq, Ord)

instance Show (SensitiveAST -> SensitiveAST) where
  show f = "SensitiveAST -> SensitiveAST"

-- Alias to represent types that are not sensitive for functions with mixed types
-- It is possible that a user defined sensitive type may not be parsed correctly into a SensitiveAST
-- In that case the user will need to alter their ParseSensitiveAST function
-- To aid the user all NonSensitiveTypes are printed and the user is instructed to verify any sensitive types
-- are not incorrectly parsed as NonSensitiveTypes
type NonSensitiveType = Type

-- Name of sensitivty environment
-- For example the name of `SDouble Diff s1` is s1
type SEnvName = String

-- Given an SEnv return the associated distance statement
type SEnvToDistance = Map SensitiveAST [GeneratedDistanceName]

-- Generates quickcheck props, tests, and a main function that runs several tests
-- Given a list of functions that will be tested against.
-- Output: main :: IO ()
genMainQuickCheck' :: ParseSensitiveAST -> String -> [Name] -> Q [Dec]
genMainQuickCheck' extractSensitiveAST mainName names = do
  testsAndProps <- mapM (genQuickCheck extractSensitiveAST) names
  let mainName' = mkName mainName
      testNames = \case { FunD testName _ -> testName } . fst <$> testsAndProps
      doStatement = DoE Nothing $ NoBindS . VarE <$> testNames
      testAndPropsList = concatMap tuple2ToList testsAndProps
  return $ FunD mainName' [Clause [] (NormalB doStatement) []] : testAndPropsList

-- Calls genMainQuickCheck' with a default instance of ParseSensitiveAST
genMainQuickCheck :: String -> [Name] -> Q [Dec]
genMainQuickCheck = genMainQuickCheck' defaultParseSensitiveASTs

-- Generates a quickcheck test for a given function
genQuickCheck :: ParseSensitiveAST -> Name -> Q (Dec, Dec)
genQuickCheck extractSensitiveAST functionName = do
  prop <- genProp' extractSensitiveAST functionName
  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      testName = mkName $ functionNameUnqualified <> "_test" -- The quickcheck function name we are generating
      (FunD propName _) = prop
  statement <- [|quickCheck (withMaxSuccess 1000 $(pure $ VarE propName))|]
  pure (FunD testName [Clause [] (NormalB statement) []], prop)

genProp :: Name -> Q Dec
genProp = genProp' defaultParseSensitiveASTs

genProp' :: ParseSensitiveAST -> Name -> Q Dec
genProp' extractSensitiveAST functionName = do
  type_ <- reifyType functionName >>= resolveTypeSynonyms
  let (unparsedTypes, typeAsts) = parseASTs type_ extractSensitiveAST
      functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      -- The name of the property function we are generating. Named [functionName]_prop
      propName = mkName $ functionNameUnqualified <> "_prop"

  -- Log parsing results
  when verbose $ liftIO $ putStrLn $ "Parsed types: " <> show typeAsts <> "\n-----"
  unless (null unparsedTypes) do
    liftIO $ putStrLn $ "Warning: The following types were not parsed as sensitive types." <>
      "Please verify they are not sensitive types."
    liftIO $ putStrLn $ "Function: " <> show functionName
    liftIO $ putStrLn $ show (length unparsedTypes) <> " Unparsed Types:\n" <> pprint unparsedTypes <> "\n-----"

  liftIO $ putStr $ show typeAsts
  inputTypeAsts <- maybe (fail noTypeAstsError) pure $ initMay typeAsts -- The input types of the function in AST form
  outputTypeAst <- maybe (fail noTypeAstsError) pure $ lastMay typeAsts -- The output of the function in AST form

  -- Generate 2 input variables and a distance variable for each Input Type
  inputTypeName <- mapM genNameForInputTypes inputTypeAsts

  -- Generate a single input variable for each non-sensitive type
  nonSensitiveInputTypeName <- replicateM (length unparsedTypes) genNameForNonSensitiveTypes

  -- Create a distance statement for each Input Type given the input arguments and distance argument
  distanceStatements <-
    mapM
      (\(ast, input1, input2, distance) -> genDistanceStatement ast distance input1 input2)
      inputTypeName

  let senvToDistance :: SEnvToDistance
      senvToDistance =
        M.fromListWith (++) $
          (\(term, _, _, distanceName) -> (term, [distanceName])) <$> inputTypeName

      -- Create the distance out statement.
      -- Gather all the arguments to the first call to F and all the arguments for the second call to F
      inputs1 = (\(_, input1, _, _) -> input1) <$> inputTypeName
      inputs2 = (\(_, _, input2, _) -> input2) <$> inputTypeName

  distanceOutStatement <- genDistanceOutStatement functionName inputs1 inputs2 nonSensitiveInputTypeName

  propertyStatement <- genPropertyStatement outputTypeAst senvToDistance
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      inputs = (\(GeneratedArgName n) -> VarP n) <$> (inputs1 <> inputs2 <> nonSensitiveInputTypeName)
      body = Clause inputs (NormalB statements) []
  pure $ FunD propName [body]
 where
  noTypeAstsError :: String
  noTypeAstsError =
    "Unable to parse "
      <> show functionName
      <> " no Sensitive Environments were found."
      <> "Perhaps you used this on the wrong function or you may need to adjust ParseSensitiveAST?"
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1"
      <*> qNewName "input2"
      <*> qNewName "distance"
  genNameForNonSensitiveTypes = GeneratedArgName <$> qNewName "nonSensitiveInput"

---- START Sensitive Environment Parser ----

type ParseError = String

-- Parses template haskell's AST and returns either a SensitiveAST or a template haskell type that failed to parse.
-- The type can potentially be a sensitive type that the user should verify.
type ParseSensitiveAST = Type -> Maybe SensitiveAST

{-
Matches TH AST to get the unparsed Sensitive Type ASTs.
This is a default instance. If a user creates a datatype that doesn't pattern match in one of these cases
Then they may implement their own SensitiveAST parser.
-}
defaultParseSensitiveASTs :: ParseSensitiveAST
defaultParseSensitiveASTs typ = do
  innerAST <- case typ of
    AppT _ innerAst ->
      Just innerAst
    _ -> Nothing
  parseInnerSensitiveAST innerAST

-- Parses sensitive ast stripped of container value.
-- e.g. This will parse (s1 +++ s2) in SDouble (s1 +++ s2) but after the SDouble is stripped.
parseInnerSensitiveAST :: Type -> Maybe SensitiveAST
parseInnerSensitiveAST typ = case typ of
  (VarT name) -> pure (SEnv_ name) -- base case captures SDouble s1
  AppT (AppT (ConT binaryOp) t1) t2 -> do
    -- recursive case
    binaryOp <- nameToBinaryOp binaryOp
    term1 <- parseInnerSensitiveAST t1
    term2 <- parseInnerSensitiveAST t2
    Just $ binaryOp term1 term2
  _ -> Nothing

-- Parses Template Haskell AST to a list of simplified ASTs
-- SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2) into a list of ASTs
parseASTs :: Type -> ParseSensitiveAST -> ([NonSensitiveType], [SensitiveAST])
parseASTs typ extractSensitiveAST = (reverse unparsedTypes, reverse sensAsts)
 where
  splitTypes = splitArgs (stripForall typ)
  (unparsedTypes, sensAsts) =
    foldl
      ( \(typeAcc, sensAstAcc) x -> case extractSensitiveAST x of
          Nothing -> (typ : typeAcc, sensAstAcc)
          Just sensAst -> (typeAcc, sensAst : sensAstAcc)
      )
      ([] :: [Type], [] :: [SensitiveAST])
      splitTypes

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

nameToBinaryOp :: Name -> Maybe (SensitiveAST -> SensitiveAST -> SensitiveAST)
nameToBinaryOp name
  | isInfixOf "+++" $ show name = pure Plus'
  | isInfixOf "JoinSens" $ show name = pure JoinSens'
  | otherwise = trace (show name) $ Nothing

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name deriving (Show, Eq, Ord)
newtype GeneratedDistanceName = GeneratedDistanceName Name deriving (Show, Eq, Ord)

-- Generates distance statement
-- e.g. d1 = abs $ unSDouble input1 - unSDouble input2
genDistanceStatement :: SensitiveAST -> GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement ast (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = Distance.distance $(pure $ VarE input1) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Name -> [GeneratedArgName] -> [GeneratedArgName] -> [GeneratedArgName] -> Q [Dec]
genDistanceOutStatement functionName inputs1 inputs2 nonSensitiveInputs =
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Applied = applyInputsOnFunction (inputs1 <> nonSensitiveInputs)
      function2Applied = applyInputsOnFunction (inputs2 <> nonSensitiveInputs)
   in [d|dout = Distance.distance $(pure function1Applied) $(pure function2Applied)|]

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- Note we need to add some small padding cause floating point artimatic
genPropertyStatement :: SensitiveAST -> SEnvToDistance -> Q Exp
genPropertyStatement ast senvToDistance = [e|dout <= $(computeRhs ast senvToDistance) + 0.00000001|]
 where
  computeRhs :: SensitiveAST -> SEnvToDistance -> Q Exp
  computeRhs sexp senvToDistance = case sexp of
    -- if it's just a sensitivity env (e.g. 's1') then return the distance value for it (e.g. 'd1')
    se@(SEnv_ sname) -> do
      distance <-
        case M.lookup se senvToDistance of
          Nothing ->
            fail $
              "Unable to find sensitivity environment: (" <> show se <> ") in distance map: " <> show senvToDistance
          Just [] ->
            fail $
              "Unable to find sensitivity environment in distance map. Empty for given key: " <> show se <>
                " in Map: " <> show senvToDistance
          Just (GeneratedDistanceName distanceName : _) -> pure distanceName
      pure $ VarE distance
    Plus' se1 se2 -> do
      nextLeft <- computeRhs se1 senvToDistance
      nextRight <- computeRhs se2 senvToDistance
      [|$(pure nextLeft) + $(pure nextRight)|]
    ScaleSens' se1 n -> do
      nextInnerExp <- computeRhs se1 senvToDistance
      [|n * $(pure nextInnerExp)|]
    JoinSens' se1 se2 -> do
      nextLeft <- computeRhs se1 senvToDistance
      nextRight <- computeRhs se2 senvToDistance
      [|max $(pure nextLeft) $(pure nextRight)|]
    _ -> fail "Undefined operation in computeRhs"

-- Pair of tuple of same type to a list of 2 elements.
tuple2ToList :: (a, a) -> [a]
tuple2ToList (a, b) = [a, b]
