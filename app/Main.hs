{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import TH
import AnnotatedExternalLibrary (f)

-- $((: []) <<= genProp 'safe_add)
-- $(genQuickCheck 'f)

main :: IO ()
main = do
  putStrLn $(stringE . pprint =<< genProp 'f)
