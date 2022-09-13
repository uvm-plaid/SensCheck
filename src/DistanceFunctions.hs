module DistanceFunctions where
import qualified Data.Matrix as Matrix


-- L2 norm of a Matrix
norm_2 :: Floating n => Matrix.Matrix n -> n
norm_2 m = sqrt $ foldr (\x acc -> acc + abs x ** 2) 0 m

-- TODO L1 https://programming-dp.com/notebooks/ch6.html#vector-valued-functions-and-their-sensitivities

absdist :: Floating n => n -> n
absdist = undefined

l1dist :: Floating n => n -> n
l1dist = undefined

l2dist :: Floating n => n -> n
l2dist = undefined

diff :: Floating n => n -> n
diff = undefined