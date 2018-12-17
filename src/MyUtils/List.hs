module MyUtils.List where

justN :: Int -> (a -> Bool) -> [a] -> Bool
justN n p xs = (length $ filter p xs) == n
