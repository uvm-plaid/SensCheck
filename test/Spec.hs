{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheck, withMaxSuccess)
import AnnotatedExternalLibrary (external_function, add_matrix_solo)
import TH (genMainQuickCheck)
import DistanceFunctions
import Utils
import Sensitivity
import MatrixCorrectedSpec (test_add_matrix_solo_corrected)


$(genMainQuickCheck "generated_tests" ['external_function, 'add_matrix_solo])

main :: IO ()
main = do
    generated_tests
    putStrLn "hiiiiiiiiii"
    test_add_matrix_solo_corrected
