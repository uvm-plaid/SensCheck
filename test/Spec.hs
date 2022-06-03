import Test.QuickCheck (quickCheck)
import Verifier (prop_distance, prop_distance_solo)

test_distance :: IO ()
test_distance = quickCheck prop_distance

test_distance_solo :: IO ()
test_distance_solo = quickCheck prop_distance_solo

main :: IO ()
main = do
  putStrLn "Testing distance compare without solo"
  test_distance
  putStrLn "Testing distance compare with solo"
  test_distance_solo
