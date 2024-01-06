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
  print x
  print y
  print z1
  print z2
  print z3
  print z4
  print "I don't do anything :). Run stack test instead."

