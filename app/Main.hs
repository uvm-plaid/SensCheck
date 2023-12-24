{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Scripts


x = $(hasSEnv)
main :: IO ()
main = do
  print x
  print "I don't do anything :). Run stack test instead."

