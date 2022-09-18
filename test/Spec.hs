{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheck, withMaxSuccess)
import AnnotatedExternalLibrary (external_function, add_matrix_solo)
import TH (genMainQuickCheck)
import DistanceFunctions
import Utils

$(genMainQuickCheck "tests" ['external_function, 'add_matrix_solo])

main :: IO ()
main = tests
