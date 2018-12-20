module MyUtils.List where

trueFor :: Int -> (a -> Bool) -> [a] -> Bool
trueFor n p xs = (length $ filter p xs) == n

trueForZeroTo :: Int -> (a -> Bool) -> [a] -> Bool
trueForZeroTo n p xs = (length $ filter p xs) <= n
