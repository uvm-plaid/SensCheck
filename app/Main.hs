{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import TH
import TestFunctions (allTypes)
import Verifier

-- $((: []) <<= genProp 'safe_add)
$(genProp 'f)

main :: IO ()
main = do
  putStrLn $(stringE . pprint =<< genProp 'allTypes)
