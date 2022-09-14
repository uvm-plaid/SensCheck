{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheck, withMaxSuccess)
import AnnotatedExternalLibrary (external_function, add_matrix_solo)
import TH (genMainQuickCheck)
import DistanceFunctions

$(genMainQuickCheck "tests" ['external_function])

main :: IO ()
main = tests
