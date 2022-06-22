{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH
import qualified Verifier

-- TODO improve error handling
genProp :: Name -> Q Dec
genProp name = do
  type_ <- reifyType name
  let asts = parseASTs type_
      propName = mkName $ "prop" <> show name
      argAsts = init asts -- The arguments to the function in AST form
      outputAst = last asts -- The output to the function in AST form
  distanceStatements <- mapM genDistanceStatement argAsts
  distanceOutStatement <- genDistanceOutStatement outputAst
  propertyStatement <- genPropertyStatement outputAst --I think this just needs the output AST
  let statements = distanceStatements <> [distanceOutStatement, propertyStatement]
      body = (\statment -> Clause [] (NormalB statment) []) <$> statements
  pure $ FunD propName body

testOutput :: IO ()
testOutput = do
  -- TODO is there a more automated way to do this?
  out <- runQ $ genProp 'Verifier.safe_add
  print out

type SEnvName = String

-- Working idea of an AST to parse into?
data Term_S
  = Plus_S Term_S Term_S -- +++
  | SEnv_S SEnvName -- SENV type argument with Label
  | SMatrix Term_S -- I need to capture SMatrix --TODO make this less flexible by only allowing Term_S?
  | SList Term_S -- Assuming the same with SList

-- >>> runQ [d|x :: (a,b,c); x = undefined|]
-- [SigD x_3 (AppT (AppT (AppT (TupleT 3) (VarT a_0)) (VarT b_1)) (VarT c_2)),ValD (VarP x_3) (NormalB (VarE GHC.Err.undefined)) []]

-- Parses SDouble Diff s -> SDouble Diff s2 -> SDouble Diff (s1 +++ s2)
-- into a list of ASTs
parseASTs :: Type -> [Term_S]
parseASTs = undefined

-- Generates
-- d1 = abs $ unSDouble y1 - unSDouble y2
-- The above uses abs but it depends on type:
-- Given a SDouble uses abs
-- Given a SMatrix uses norm function
-- Given an SList uses TODO
genDistanceStatement :: Term_S -> Q Exp
genDistanceStatement = undefined

-- Generates
-- dout = abs $ unSDouble (f x1 y1) - unSDouble (f x2 y2)
-- Same rule for replacing abs as genDistanceStatement
-- TODO what if it's a 3 function argument or 1.
-- dout = abs $ unSDouble (f x1 y1 z1) - unSDouble (f x2 y2 z1)
genDistanceOutStatement :: Term_S -> Q Exp
genDistanceOutStatement = undefined

-- Generates:
--  dout <= [[ sensitivity_expression ]]
-- for example if the output is: s1 +++ s2 then we assert d1 + d2
-- But also add some small padding cause floating point artimatic
genPropertyStatement :: Term_S -> Q Exp
genPropertyStatement = undefined
