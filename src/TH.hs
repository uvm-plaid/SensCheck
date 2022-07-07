{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (unless, zipWithM)
import Data.Coerce (coerce)
import Data.List (transpose)
import Data.Map
import qualified Data.Map as M
import Data.Traversable (for)
import Debug.Trace (trace)
import GHC.Base (Nat)
import qualified GHC.Num
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName (ModName), Name (Name), NameFlavour (NameQ), qNewName)
import Sensitivity (CMetric (L1, L2), NMetric (Diff, Disc))
import qualified Sensitivity
import qualified Verifier

type SEnvName = String

-- Working idea of an AST to parse into
-- TODO this allows invalid representations
data Term_S
  = Plus_S Term_S Term_S -- +++
  | SEnv_S SEnvName -- TODO make this less flexible by only allowing Term_S?
  | SDouble_S NMetric Term_S -- Assuming I need to capture this
  | SMatrix_S CMetric Term_S
  | SList_S CMetric Term_S -- Assuming I need to capture this
  | ScaleSens_S Term_S Int
  deriving (Show)

-- TODO improve error handling
-- TODO I think I need a LetE
-- TODO clean this up
genProp :: Name -> Q Dec
genProp functionName = do
  type_ <- reifyType functionName
  let typeAsts = parseASTs type_
      propName = mkName $ show functionName <> "_prop" -- The quickcheck property function name we are generatinge
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
  pure $ FunD propName [body]
 where
  genNameForInputTypes ast =
    (\input1 input2 distance -> (ast, GeneratedArgName input1, GeneratedArgName input2, GeneratedDistanceName distance))
      <$> qNewName "input1" <*> qNewName "input2" <*> qNewName "distance"

-- >>> runQ [d|x :: (a,b,c); x = undefined|]
-- [SigD x_3 (AppT (AppT (AppT (TupleT 3) (VarT a_0)) (VarT b_1)) (VarT c_2)),ValD (VarP x_3) (NormalB (VarE GHC.Err.undefined)) []]

-- This might be the output of SDouble s1 -> SDouble s2 -> SDouble (s1 +++ s2)
-- You might notice I'm ignoring SDouble. Not sure if we really need to capture that.
exampleAstPlus :: [Term_S]
exampleAstPlus =
  [ SDouble_S Diff $ SEnv_S "s1"
  , SDouble_S Diff $ SEnv_S "s2"
  , SDouble_S Diff $ Plus_S (SEnv_S "s1") (SEnv_S "s2")
  ]

exampleAstDiscardS2 :: [Term_S]
exampleAstDiscardS2 =
  [ SDouble_S Diff $ SEnv_S "s1"
  , SDouble_S Diff $ SEnv_S "s2"
  , SDouble_S Diff $ SEnv_S "s1"
  ]

-- Parses SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
-- into a list of ASTs
-- This is going to be painful
parseASTs :: Type -> [Term_S]
parseASTs typ = exampleAstPlus -- Mock value. TODO figure out how to implement this later.

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name
newtype GeneratedDistanceName = GeneratedDistanceName Name

-- Generates
-- d1 = abs $ unSDouble input1 - unSDouble input2
-- The above uses abs but it depends on type:
-- Given a SDouble uses abs
-- Given a SMatrix uses norm function
-- Given an SList uses TODO
genDistanceStatement :: Term_S -> GeneratedDistanceName -> GeneratedArgName -> GeneratedArgName -> Q [Dec]
genDistanceStatement ast (GeneratedDistanceName distance) (GeneratedArgName input1) (GeneratedArgName input2) =
  [d|$(pure $ VarP distance) = $(τ ast) $(pure $ VarE input1) $(pure $ VarE input2)|]

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- TODO what if it's a 3 function argument or 1.
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Term_S -> Name -> [GeneratedArgName] -> [GeneratedArgName] -> Q ([Dec], GeneratedDistanceName)
genDistanceOutStatement ast functionName inputs1 inputs2 = do
  distance <- qNewName "distanceOut"
  -- Recursively apply all inputs on function
  let applyInputsOnFunction :: [GeneratedArgName] -> Exp
      applyInputsOnFunction args = Prelude.foldl (\acc arg -> AppE acc (VarE arg)) (VarE functionName) (coerce <$> args)
      function1Application = applyInputsOnFunction inputs1
      function2Application = applyInputsOnFunction inputs2
  statement <- case ast of
    SDouble_S Diff _ -> [d|$(pure $ VarP distance) = abs $ unSDouble $(pure function1Application) - unSDouble $(pure function2Application)|]
    SMatrix_S L2 _ -> [d|$(pure $ VarP distance) = norm_2 $ unSDouble $(pure function1Application) - unSDouble $(pure function2Application)|]
    _ -> fail $ "Unexpected input in genDistanceStatement AST: " <> show ast
  pure
    ( statement
    , GeneratedDistanceName distance
    )

τ :: Term_S -> Q Exp
τ ast = case ast of
  SDouble_S Diff _ -> [|absdist|]
  SDouble_S Disc _ -> [|diff|]
  SMatrix_S L2 (SDouble_S Diff _) -> [|l2dist|]
  SMatrix_S L1 (SDouble_S Diff _) -> [|l1dist|]
  SMatrix_S L2 (SDouble_S Disc _) -> [|l2dist . diff|]
  SMatrix_S L2 (SDouble_S Disc _) -> [|l1dist . diff|]
  SList_S L2 (SDouble_S Diff _) -> [|l2dist|]
  SList_S L1 (SDouble_S Diff _) -> [|l1dist|]
  SList_S L2 (SDouble_S Disc _) -> [|l2dist . diff|]
  SList_S L2 (SDouble_S Disc _) -> [|l1dist . diff|]
  _ -> fail $ "Unexpected input in τ AST: " <> show ast

-- Map to SEnv to Distance Name
-- TODO I need this for below
type SEnvToDistanceName = Map SEnvName Name

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- But also add some small padding cause floating point artimatic
-- Notice I need all the distance names here
-- And need to associate them to sensitivity values
genPropertyStatement :: Term_S -> Q Exp
genPropertyStatement ast = do
  [e|dout <= $(computeRhs ast)|]
 where
  computeRhs :: Term_S -> Q Exp
  computeRhs sexp = case sexp of
    -- if it's just a sensitivity env (e.g. 's1') then return the distance value for it (e.g. 'd1')
    SEnv_S s -> [|s|]
    SDouble_S _ se -> computeRhs se
    Plus_S se1 se2 -> [|$(computeRhs se1) + $(computeRhs se2)|]
    ScaleSens_S se1 n -> [|n * $(computeRhs se1)|]
    _ -> undefined
