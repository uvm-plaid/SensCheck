{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (unless, zipWithM)
import Data.Traversable (for)
import qualified GHC.Num
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qNewName)
import qualified Sensitivity
import qualified Verifier

type SEnvName = String

-- Working idea of an AST to parse into?
data Term_S
  = Plus_S Term_S Term_S -- +++
  | SEnv_S SEnvName -- SENV type argument with Label
  | SMatrix Term_S -- I need to capture SMatrix --TODO make this less flexible by only allowing Term_S?
  | SList Term_S -- Assuming the same with SList
  deriving (Show, Eq)

-- TODO improve error handling
-- TODO I think I need a LetE
genProp :: Name -> Q Dec
genProp name = do
  type_ <- reifyType name
  let asts = parseASTs type_
      propName = mkName $ "prop" <> show name
      argAsts = init asts -- The arguments to the function in AST form
      outputAst = last asts -- The output to the function in AST form
  (distanceStatements, generatedArgNames, generatedDistanceNames) <- unzip3 <$> mapM genDistanceStatement argAsts
  (distanceOutStatement, generatedDistanceOutName) <- genDistanceOutStatement outputAst
  propertyStatement <- genPropertyStatement outputAst --I think this just needs the output AST but also the distance names
  let statements = LetE (distanceStatements <> [distanceOutStatement]) propertyStatement
      body = Clause [] (NormalB statements) []
  pure $ FunD propName [body]

testOutput :: IO ()
testOutput = do
  -- TODO is there a more automated way to do this?
  out <- runQ $ genProp 'Verifier.safe_add
  print out

-- >>> runQ [d|x :: (a,b,c); x = undefined|]
-- [SigD x_3 (AppT (AppT (AppT (TupleT 3) (VarT a_0)) (VarT b_1)) (VarT c_2)),ValD (VarP x_3) (NormalB (VarE GHC.Err.undefined)) []]

-- Parses SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
-- into a list of ASTs
parseASTs :: Type -> [Term_S]
parseASTs = undefined

-- Represents generated Arguments
newtype GeneratedArgName = GeneratedArgName Name
newtype GeneratedDistanceName = GeneratedDistanceName Name

-- Generates
-- d1 = abs $ unSDouble y1 - unSDouble y2
-- The above uses abs but it depends on type:
-- Given a SDouble uses abs
-- Given a SMatrix uses norm function
-- Given an SList uses TODO
genDistanceStatement :: Term_S -> Q (Dec, [GeneratedArgName], GeneratedDistanceName)
genDistanceStatement ast =
  do
    input1 <- qNewName "input1"
    input2 <- qNewName "input2"
    distance <- qNewName "distance"
    -- Function to operate on and unwrapping function
    (op, unwrap) <- case ast of
      SEnv_S _ -> pure (VarE 'GHC.Num.abs, VarE 'Sensitivity.unSDouble)
      SMatrix _ -> pure (VarE 'Verifier.norm_2, VarE 'Verifier.toDoubleMatrix)
      SList _ -> _ --TODO idk
      _ -> fail $ "Unexpected input in genDistanceStatement AST: " <> show ast
    let statement = ValD (VarP distance) (NormalB undefined) []
    pure
      ( statement
      , [GeneratedArgName input1, GeneratedArgName input2]
      , GeneratedDistanceName distance
      )

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- TODO what if it's a 3 function argument or 1.
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Term_S -> Q (Dec, GeneratedDistanceName)
genDistanceOutStatement = undefined

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- But also add some small padding cause floating point artimatic
-- Notice I need all the distance names here
genPropertyStatement :: Term_S -> Q Exp
genPropertyStatement = undefined
