{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TH where

import Control.Monad (replicateM, unless, when, zipWithM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.List (isInfixOf, transpose)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Set (Set (..))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Debug.Trace qualified as Debug
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
import Data.Foldable (fold)
import Sensitivity qualified
import Primitives (Unsafe(..))

-- TODO remove Debug.traces when complete
debug = True

trace :: String -> a -> a
trace = if debug then Debug.trace else \_ a -> a

traceM :: Monad m => String -> m ()
traceM = if debug then Debug.traceM else \_ -> pure ()

-- Enable verbose logging
verbose :: Bool
verbose = False

-- Padding due to floating point imprecision
-- This demonstrates a mismatch between implementation
padding = 0.00000001

data SensitiveAST
  = SEnv_ Name -- Terminal Sensitivity Environment
  | TyLit_ Integer -- Terminal Type Literal (e.g. dependently typed 1)
  | Plus' SensitiveAST SensitiveAST -- +++
  | JoinSens' SensitiveAST SensitiveAST -- Max(s1, s2)
  | ScaleSens' SensitiveAST Integer-- n * senv
  | TruncateSens' SensitiveAST -- Truncate Sensitivity
  | SFun SensitiveAST SensitiveAST -- Function
  deriving (Show, Eq, Ord)

instance Show (SensitiveAST -> SensitiveAST) where
  show :: (SensitiveAST -> SensitiveAST) -> String
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

-- Calls sensCheck' with a default instance of ParseSensitiveAST
sensCheck :: String -> [Name] -> Q [Dec]
sensCheck = sensCheck' defaultParseSensitiveASTs

-- Generates quickcheck props, tests, and a main function that runs several tests
-- Given a list of functions that will be tested against.
-- Output: main :: IO ()
sensCheck' :: ParseSensitiveAST -> String -> [Name] -> Q [Dec]
sensCheck' parseSensitiveAST mainName names = do
  testsAndProps <- mapM (genQuickCheck parseSensitiveAST) names
  let mainName' = mkName mainName
      testNames = \case { FunD testName _ -> testName } . fst <$> testsAndProps
      doStatement = DoE Nothing $ NoBindS . VarE <$> testNames
      testAndPropsList = concatMap tuple2ToList testsAndProps
  return $ FunD mainName' [Clause [] (NormalB doStatement) []] : testAndPropsList

-- Generates a quickcheck test for a given function
genQuickCheck :: ParseSensitiveAST -> Name -> Q (Dec, Dec)
genQuickCheck parseSensitiveAST functionName = do
  prop <- genProp' parseSensitiveAST functionName
  let functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      testName = mkName $ functionNameUnqualified <> "Test" -- The quickcheck function name we are generating
      (FunD propName _) = prop
  statement <- [|quickCheck (withMaxSuccess 100 $(pure $ VarE propName))|]
  pure (FunD testName [Clause [] (NormalB statement) []], prop)

-- Generate just the property
-- In most cases you may wish to use sensCheck for convenience
-- This might be useful if for example you want to pass arguments (that you might need IO for)
sensProperty :: Name -> Q Dec
sensProperty = genProp' defaultParseSensitiveASTs

genProp' :: ParseSensitiveAST -> Name -> Q Dec
genProp' parseSensitiveAST functionName = do
  type_ <- reifyType functionName >>= resolveTypeSynonyms
  let (unparsedTypes, typeAsts) = parseASTs type_ parseSensitiveAST
      functionNameUnqualified = reverse $ takeWhile (/= '.') $ reverse $ show functionName
      -- The name of the property function we are generating. Named [functionName]Prop
      propName = mkName $ functionNameUnqualified <> "Prop"

  -- Log parsing results
  when verbose $ liftIO $ putStrLn $ "Parsed types: " <> show typeAsts <> "\n-----"
  unless (null unparsedTypes && verbose) do
    liftIO $
      putStrLn $
        "Warning: The following types were not parsed as sensitive types. "
          <> "Please verify they are not sensitive types."
    liftIO $ putStrLn $ "Function: " <> show functionName <> "\n"
    liftIO $ putStrLn $ show (length unparsedTypes) <> " Unparsed Type(s):\n" <> pprint unparsedTypes <> "\nDone Unparsed Types\n-----"

  liftIO $ putStr $ show typeAsts
  inputTypeAsts <- maybe (fail noTypeAstsError) pure $ initMay typeAsts -- The input types of the function in AST form
  outputTypeAst <- maybe (fail noTypeAstsError) pure $ lastMay typeAsts -- The output of the function in AST form

  -- Generate 2 input variables and a distance variable for each Input Type except functions
  inputTypeNames <- mapM genNameForInputType inputTypeAsts

  -- Generate a single input variable for each non-sensitive type
  nonSensitiveInputTypeName <- replicateM (length unparsedTypes) genNameForNonSensitiveTypes

  -- Create a distance statement for each Input Type given the input arguments and distance argument
  distanceStatements <- mapM
      (\case GenInput input1 input2 distance _ -> genDistanceStatement distance input1 input2
             GenFunction n _ -> pure [])
      inputTypeNames

  let senvToDistance :: SEnvToDistance
      senvToDistance = M.fromListWith (++) $
        (\case GenInput _ _ distance term -> (term, [distance]); GenFunction _ term -> (term, [])) <$> inputTypeNames

      -- Create the distance out statement.
      -- Gather all the arguments to the first call to F and all the arguments for the second call to F
      inputs1 = mapMaybe (\case GenInput input1 _ _ _ -> Just input1; _ -> Nothing) inputTypeNames
      inputs2 = mapMaybe (\case GenInput _ input2 _ _ -> Just input2; _ -> Nothing) inputTypeNames

  distanceOutStatement <- genDistanceOutStatement functionName inputTypeNames nonSensitiveInputTypeName

  propertyStatement <- genPropertyStatement outputTypeAst senvToDistance
  let statements = LetE (concat distanceStatements <> distanceOutStatement) propertyStatement
      hasSFunction = any (\case GenFunction _ _ -> True; _ -> False) inputTypeNames
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
  genNameForInputType ast = case ast of
    SFun _ _ -> pure $ GenFunction (sfunctionLength ast) ast
    _ -> (\input1 input2 distance -> GenInput (GeneratedArgName input1) (GeneratedArgName input2) (GeneratedDistanceName distance) ast)
          <$> qNewName "input1"
          <*> qNewName "input2"
          <*> qNewName "distance"
  genNameForNonSensitiveTypes = GeneratedArgName <$> qNewName "nonSensitiveInput"

sfunctionLength :: SensitiveAST -> Integer
sfunctionLength (SFun a b) = 1 + sfunctionLength a + sfunctionLength b
sfunctionLength _ = 0

-- Sum type for generated names for inputs
-- Or a function with n inputs
data GeneratedInputOrFunction = GenInput GeneratedArgName GeneratedArgName GeneratedDistanceName SensitiveAST | GenFunction Integer SensitiveAST

---- START Sensitive Environment Parser ----

type ParseError = String

-- Parses template haskell's AST and returns either a SensitiveAST or a template haskell type that failed to parse.
-- The type can potentially be a sensitive type that the user should verify.
type ParseSensitiveAST = SensitiveTypeParams -> Type -> Maybe SensitiveAST

type SensitiveTypeParams = Set Name

{-
Matches TH AST to get the unparsed Sensitive Type ASTs.
This is a default instance. If a user creates a datatype that doesn't pattern match in one of these cases
Then they may implement their own SensitiveAST parser.
-}
defaultParseSensitiveASTs :: ParseSensitiveAST
defaultParseSensitiveASTs typeParams typ = do
  -- TODO Why was I doing this?
  -- innerAST <- case typ of
  --   AppT _ innerAst ->
  --     Just innerAst
  --   _ -> trace ("Match on Nothing in defaultParseSensitiveASTs " <> show typ) Nothing
  -- parseInnerSensitiveAST typeParams innerAST
  parseInnerSensitiveAST typeParams typ

-- Parses sensitive ast stripped of container value.
-- e.g. This will parse (s1 +++ s2) in SDouble (s1 +++ s2) but after the SDouble is stripped.
parseInnerSensitiveAST :: SensitiveTypeParams -> Type -> Maybe SensitiveAST
parseInnerSensitiveAST typeParams typ = trace ("+++Starting Parsing for \ntyp " <> pprint typ <> "\n typ as TH " <> show typ <> "\n===\n") -- <> show typeParams <> "\n\n----\n")
  case typ of
    AppT _ (VarT name) -> if Set.member name typeParams then Just (SEnv_ name) else Nothing
    -- Redundent?
    -- How is SList parsed well probbly the above
    -- AppT (AppT (AppT (ConT Sensitivity.SList) (PromotedT Sensitivity.L1)) (AppT (ConT Sensitivity.SDouble) (PromotedT Sensitivity.Diff)))

    -- (VarT s4_6989586621679054201)

    VarT name -> if Set.member name typeParams then Just (SEnv_ name) else Nothing
    LitT (NumTyLit n) -> pure (TyLit_ n)
    -- Matches on t1 -> t2
    -- Then parses t1 and t2 recursively
    -- e.g. a 2 arg function:
    -- SDouble Diff s -> SDouble Diff (s2 :: SEnv) -> SDouble Diff (s1 +++ s2)
    -- Parses as: SFun (SEnv_ s) (SFun (SEnv_ s2) (SEnv_ s1))
    AppT (AppT ArrowT t1) t2 -> do
      traceM $ "Match on ArrowT \nt1: " <> show t1 <> "\n\nt2: " <> show t2
      term1 <- parseInnerSensitiveAST typeParams t1
      term2 <- parseInnerSensitiveAST typeParams t2
      traceM $ "Match on inner ArrowT SUCESS Parsed \nt1: " <> show term1 <> "\n\nt2: " <> show term2
      Just $ SFun term1 term2
    -- Matches on binaryOpWithLit term n
    -- Where n is a numeric literal
    -- Then searches for matching binaryOpWithLit in SensitiveAST of type SensitiveAST -> Integer -> SensitiveAST
    -- e.g. ScaleSens s n parses as ScaleSens' (SEnv_ s) (TyLit_ n)
    -- TODO doesn't seem to match on 2nd term in
    -- Sensitivity.SDouble 'Sensitivity.Diff s1_0 -> Sensitivity.SDouble 'Sensitivity.Diff (Sensitivity.ScaleSens s1_0 1)
    -- Namely Sensitivity.SDouble 'Sensitivity.Diff (Sensitivity.ScaleSens s1_0 1)
    -- AppT (AppT (ConT Sensitivity.SDouble) (PromotedT Sensitivity.Diff))
    --      (AppT (AppT (ConT Sensitivity.ScaleSens) (VarT s1_6989586621679054334)) (LitT (NumTyLit 1)))
    -- Hmm so I can see why this matches in the other case since it starts with AppT


    AppT (AppT (ConT binaryOpWithLit) term) (LitT (NumTyLit n)) -> do
      traceM ("Match on binaryOpWithLit " <> show binaryOpWithLit)
      binaryOpWithLit <- nameToBinaryOpWithLit binaryOpWithLit
      term' <- parseInnerSensitiveAST typeParams term
      Just $ binaryOpWithLit term' n
    -- TODO copying this stuct too
    AppT debugFst@_ debugSnd@(AppT (AppT (ConT binaryOpWithLit) term) (LitT (NumTyLit n))) -> do
      traceM ("Match on binaryOpWithLit " <> show binaryOpWithLit)
      binaryOpWithLit <- nameToBinaryOpWithLit binaryOpWithLit
      term' <- parseInnerSensitiveAST typeParams term
      Just $ binaryOpWithLit term' n
    -- Matches on binaryOp term1 term2
    -- Then searches for matching binaryOp in SensitiveAST of type SensitiveAST -> SensitiveAST -> SensitiveAST
    -- e.g. s1 +++ s2 parses as Plus' (SEnv_ s1) (SEnv_ s2)
    AppT debugFst@_ debugSnd@(AppT (AppT (ConT binaryOp) t1) t2) -> do
      traceM ("Match on Cont " <> show binaryOp)
      traceM $ "Ignoring first part: " <> show debugFst <> "\n pprint; " <> pprint debugFst <> "\n\n 2nd" <> show debugSnd <> "\n\n" <> pprint debugSnd
      -- recursive case
      binaryOp <- nameToBinaryOp binaryOp
      term1 <- parseInnerSensitiveAST typeParams t1
      term2 <- parseInnerSensitiveAST typeParams t2
      Just $ binaryOp term1 term2
    -- Matches on unaryOp term
    -- Then searches for matching unaryOp in SensitiveAST of type SensitiveAST -> SensitiveAST
    -- e.g. TruncateSens s1 parses as TruncateSens' s1
    AppT _ (AppT (ConT unaryOp) t) -> do
      traceM ("Match on unaryOp " <> show unaryOp)
      unaryOp <- nameToUnaryOp unaryOp
      -- recursive case
      term <- parseInnerSensitiveAST typeParams t
      Just $ unaryOp term
    -- Might be a higher order function
    ForallT _ _ t' -> do
      -- Collect for whole term
      let (t'', typeParams') = collectSEnvTypeParams typ
      let typeParams'' = Set.union typeParams typeParams'
      traceM $ "Match on inner ForallT \nt: " <> show typ <> "\n\nt'': " <> show t'' <> "\n\nUpdated typeParams: " <> show typeParams''
      parseInnerSensitiveAST typeParams'' t'
    _ -> trace ("\nMatch on Nothing for " <> show typ <> "\n" ) Nothing

-- Parses Template Haskell AST to a list of simplified ASTs
-- SDouble Diff s -> SDouble Diff (s2 :: SEnv) -> SDouble Diff (s1 +++ s2) into a list of ASTs
parseASTs :: Type -> ParseSensitiveAST -> ([NonSensitiveType], [SensitiveAST])
parseASTs typ parseSensitiveAST = (reverse unparsedTypes, reverse sensAsts)
 where
  (typ', sensitiveTypeParams) = collectSEnvTypeParams typ
  splitTypes = splitArgs typ'
  (unparsedTypes, sensAsts) = trace ("splitTypes " <> (show $ show <$> splitTypes) <> "type params" <> show sensitiveTypeParams) $
    foldl
      ( \(typeAcc, sensAstAcc) x -> case parseSensitiveAST sensitiveTypeParams x of
          Nothing -> (x : typeAcc, sensAstAcc)
          Just sensAst -> (typeAcc, sensAst : sensAstAcc)
      )
      ([] :: [Type], [] :: [SensitiveAST])
      splitTypes


-- Collect Sensitive type params names and return the typed stripped of the forall if any
collectSEnvTypeParams :: Type -> (Type, Set Name)
collectSEnvTypeParams type_ = case type_ of
  ForallT typeParams _ t' ->
    (t',
      foldr (\t acc -> case t of
        KindedTV name _ (ConT kind) -> if isSensitive kind then acc <> Set.singleton name else acc
        KindedTV name _ t' -> if appT t' then acc <> Set.singleton name else Set.empty
        _ -> acc
      ) Set.empty (Set.fromList typeParams)
    )
  t' -> (t', Set.empty)
  where
    -- Search through AppT
    appT t =
      case t of
        (AppT t1 (ConT kind)) -> isSensitive kind || appT t1
        -- More likely to be in t2
        (AppT t1 t2) -> appT t2 || appT t1
        _ -> False
    isSensitive :: Name -> Bool
    isSensitive name = show name == "Sensitivity.SEnv" || show name == "Sensitivity.Sensitivity"

-- Split when encountering ->
splitArgs :: Type -> [Type]
splitArgs typ = case typ of
  AppT (AppT ArrowT t1) t2 -> splitArgs t1 ++ splitArgs t2
  t -> [t]

nameToBinaryOp :: Name -> Maybe (SensitiveAST -> SensitiveAST -> SensitiveAST)
nameToBinaryOp name
  | isInfixOf "+++" $ show name = pure Plus'
  | isInfixOf "JoinSens" $ show name = pure JoinSens'
  | otherwise = trace ("Missing binary op: " <> show name) Nothing

nameToUnaryOp :: Name -> Maybe (SensitiveAST -> SensitiveAST)
nameToUnaryOp name
  | isInfixOf "TruncateSens" $ show name = pure TruncateSens'
  | otherwise = trace ("Missing unary op: " <> show name) Nothing

nameToBinaryOpWithLit :: Name -> Maybe (SensitiveAST -> Integer -> SensitiveAST)
nameToBinaryOpWithLit name
  | isInfixOf "ScaleSens" $ show name = pure ScaleSens'
  | otherwise = trace ("Missing binary op with lit: " <> show name) Nothing

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name deriving (Show, Eq, Ord)
newtype GeneratedDistanceName = GeneratedDistanceName Name deriving (Show, Eq, Ord)

-- Generates distance statement
-- e.g. d1 = abs $ unSDouble input1 - unSDouble input2
genDistanceStatement :: GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = Distance.distance $(pure $ VarE input1) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
-- TODO preserve order of inputs and apply in order
-- -- This currently forces users to put non-sensitive inputs at the end
genDistanceOutStatement :: Name -> [GeneratedInputOrFunction] -> [GeneratedArgName] -> Q [Dec]
genDistanceOutStatement functionName inputs nonSensitiveInputs =
  -- Recursively apply all inputs on function
  let applyInputsOnFunction = Prelude.foldl AppE (VarE functionName)
      sensitiveInputs1 = (\case GenInput input1 _ _ _ -> VarE $ coerce input1; GenFunction n ast -> processHof ast) <$> inputs
      sensitiveInputs2 = (\case GenInput _ input2 _ _ -> VarE $ coerce input2; GenFunction n ast -> processHof ast) <$> inputs
      nonSensitiveInputs' = VarE . coerce <$> nonSensitiveInputs
      function1Applied = applyInputsOnFunction (sensitiveInputs1 <> nonSensitiveInputs')
      function2Applied = applyInputsOnFunction (sensitiveInputs2 <> nonSensitiveInputs')
   in [d|dout = Distance.distance $(pure function1Applied) $(pure function2Applied)|]
   where
     -- Generates a function table for higher order functions
     -- This could be improved to handle more unusual cases but for now it's sufficient
     processHof :: SensitiveAST -> Exp
     processHof ast = case ast of
        SFun (SEnv_ name) b -> LamE [VarP name ] $ processHof b
        _ -> AppE (VarE 'wrap) (processS ast)

     mkSEnvToProxy :: SensitiveAST -> (Set Name, Map Name [Integer]) -> (Set Name, Map Name [Integer])
     mkSEnvToProxy ast acc@(args, senvAcc) = case ast of
        SFun a b -> mkSEnvToProxy b $ mkSEnvToProxy a acc
        SEnv_ name -> (args <> Set.singleton name, senvAcc)
        ScaleSens' (SEnv_ name) n -> (args, M.insertWith (++) name [n] senvAcc)
        Plus' a b -> mkSEnvToProxy a acc <> mkSEnvToProxy b acc
        -- not supported yet
        _ -> (args, senvAcc)

processS :: SensitiveAST -> Exp
processS ast = case ast of
  ScaleSens' a n -> AppE (AppE (VarE '(*)) (LitE $ IntegerL n)) (processS a)
  SEnv_ name -> AppE (VarE 'unwrap) (VarE name)
  TyLit_ n -> LitE $ IntegerL n
  Plus' a b -> AppE (AppE (VarE '(+)) (processS a)) (processS b)
  JoinSens' a b -> AppE (AppE (VarE 'max) (processS a)) (processS b)

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- Note we need to add some small padding cause floating point artimatic
genPropertyStatement :: SensitiveAST -> SEnvToDistance -> Q Exp
genPropertyStatement ast senvToDistance = [e|dout <= $(computeRhs ast senvToDistance) + padding|]
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
              "Unable to find sensitivity environment in distance map. Empty for given key: "
                <> show se
                <> " in Map: "
                <> show senvToDistance
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
    _ -> fail $ "Undefined operation in computeRhs" <> show sexp

-- Pair of tuple of same type to a list of 2 elements.
tuple2ToList :: (a, a) -> [a]
tuple2ToList (a, b) = [a, b]
