{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Scripts
import AnnotatedExternalLibrary
import DpMinst

x = $(hasSEnv 'solo_plus)
y = $(hasSEnv 'DpMinst.clippedGrad)
z1 = $(hasSEnv 'solo_mixed_types_mult_generic)
z2 = $(hasSEnv 'add_pair_solo)
z3 = $(hasSEnv 'add_dependently_typed_matrix_solo)
z4 = $(hasSEnv 'add_matrix_solo)

main :: IO ()
main = do
  net0 <- randomMnist
  putStrLn $ "rando mnist: " <> show net0
  putStrLn $ "Result: " <> (show $ trainingClippedGradDistance net0)

