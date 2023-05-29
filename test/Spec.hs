{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import AnnotatedExternalLibrary (add_dependently_typed_matrix_solo, add_matrix_solo, add_pair_solo, solo_plus, solo_plus_incorrect, solo_mixed_types)
import GHC.TypeLits (KnownNat)
import Sensitivity
import TH (genMainQuickCheck)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils

f = add_dependently_typed_matrix_solo @2 @4
$(genMainQuickCheck "tests" ['solo_plus, 'add_pair_solo, 'f, 'solo_mixed_types])


-- TODO get rid of these later
-- $(do
--   type_ <- reifyType 'solo_plus
--   type_' <- resolveTypeSynonyms type_
--   stringE $ show $ parseASTs type_' defaultParseSensitiveASTs
-- )

-- $(
-- let inputAst = (SEnv_ $ mkName "s1")
--     -- Create 2 arguments with the type t1 s1
--     firstArgName = (GeneratedArgName $ mkName "s1_a")
--     secondArgName = (GeneratedArgName $ mkName "s1_b") in
--     -- This will be the distance of the above two arguments
--     distanceNameForInput = (GeneratedDistanceName $ mkName "dist_for_s1")
--     do
--       statement <- genDistanceStatement firstInputAst distanceNameForFirstInput firstInputArgName secondInputArgName
--       stringE $ pprint statement
-- )

-- $(
-- let inputAst = (SEnv_ $ mkName "s1")
--     -- Create 2 arguments with the type X that contain the above senv
--     firstArgName = (GeneratedArgName $ mkName "x1")
--     secondArgName = (GeneratedArgName $ mkName "x2") in
--     -- This will be the distance of the above two arguments
--     distanceNameForInput = (GeneratedDistanceName $ mkName "dist_for_s1")
--     do
--       statement <- genDistanceStatement firstInputAst distanceNameForFirstInput firstInputArgName secondInputArgName
--       stringE $ pprint statement
-- )

-- $(
-- let -- Create 2 arguments with the type X and Y
--     type1Args = [(GeneratedArgName $ mkName "x1"), (GeneratedArgName $ mkName "y1")]
--     -- Create another 2 arguments with the type X and Y
--     type2Args = [(GeneratedArgName $ mkName "x2"), (GeneratedArgName $ mkName "y2")]
--     functionName = 'solo_plus in
--     do
--       statement <- genDistanceOutStatement functionName type1Args type2Args
--       stringE $ pprint statement
-- )

-- genDistanceStatement (SEnv_ $ mkName "s1") (GeneratedDistanceName $ mkName "dist_for_s1") (GeneratedArgName $ mkName "s1_a") (GeneratedArgName $ mkName "s1_b")

$(genMainQuickCheck "failing_tests" ['add_matrix_solo, 'solo_plus_incorrect])

main :: IO ()
main = do
  putStrLn "\n\nThese tests are expected to pass:"
  tests
  putStrLn "\n\n=================================="
  putStrLn "These tests are expected to fail:\n\n"
  failing_tests
