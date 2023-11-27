module Main where

import Language.Haskell.TH
import TH
import DpMinst (testHMatrixFoldr)


main :: IO ()
main = do
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  testHMatrixFoldr
  print "I don't do anything :). Run stack test instead."

