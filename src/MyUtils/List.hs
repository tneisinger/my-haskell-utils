module MyUtils.List where

import Data.List (genericLength)

{-|
  Given a number n, a predicate p, and a list xs, return True
  if the predicate is true for exactly n of the values in xs.

  Examples:

    > trueFor 2 (>4) [2,5,1,7]
    True

    > trueFor 3 (>4) [2,5,1,7]
    False
-}
trueFor :: (Eq a, Num a) => a -> (b -> Bool) -> [b] -> Bool
trueFor n p xs = (genericLength $ filter p xs) == n

{-|
  Given a number n, a predicate p, and a list xs, return True
  if p is true for n or fewer of the values in xs.

  Examples:

    > trueForZeroTo 2 (>4) [2,5,1,7]
    True

    > trueForZeroTo 2 (>4) [2,3,1,6]
    True

    > trueForZeroTo 3 (>4) [9,5,1,6,7]
    False
-}
trueForZeroTo :: (Ord a, Num a) => a -> (b -> Bool) -> [b] -> Bool
trueForZeroTo n p xs = (genericLength $ filter p xs) <= n
