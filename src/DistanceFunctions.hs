module DistanceFunctions where

import Data.Matrix qualified as Matrix

-- L2 norm of a Matrix
norm_2 :: Floating n => Matrix.Matrix n -> n
norm_2 m = sqrt $ foldr (\x acc -> acc + abs x ** 2) 0 m

-- TODO I think I am confusing norms and distances.
-- Review that and remove l1dist OR l1norm here?
-- Joe called this the l1 norm though, so I think it's right.
l1norm :: (Traversable f, Floating n) => f n -> n
l1norm = sum

-- TODO L1 https://programming-dp.com/notebooks/ch6.html#vector-valued-functions-and-their-sensitivities

-- TODO verify these with Joe

absdist :: Floating n => n -> n -> n
absdist x y = abs $ x - y

l1dist :: (Traversable f, Floating n) => f n -> n
l1dist = sum

l2dist :: (Traversable f, Floating n) => f n -> n
l2dist n = sqrt $ foldl (\acc x -> x ** 2 + acc) 0 n

diff :: Floating n => n -> n
diff = undefined

l2norm :: (Traversable f, Floating n) => f n -> n
l2norm n = sqrt $ foldl (\acc x -> x ** 2 + acc) 0 n
