{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import AnnotatedExternalLibrary (add_matrix_solo, solo_plus)
import DistanceFunctions
import Sensitivity
import TH (genMainQuickCheck)
import Test.QuickCheck (quickCheck, withMaxSuccess)
import Utils

$(genMainQuickCheck "tests" ['solo_plus, 'add_matrix_solo])

main :: IO ()
main = tests
