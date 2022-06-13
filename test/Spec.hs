import Test.QuickCheck (quickCheck, withMaxSuccess)
import Verifier (prop_distance, prop_distance_solo, prop_safe_add, prop_safe_add_solo)
import VerifierQuickCheck (test_add, test_add_solo, test_distance, test_distance_solo)

main :: IO ()
main = do
  putStrLn "Testing distance compare without solo"
  test_distance
  putStrLn "Testing distance compare with solo"
  test_distance_solo
  putStrLn "Testing matrix addition without solo"
  test_add
  putStrLn "Testing matrix addition with solo"
  test_add_solo
