module MyUtils.Show where

import Data.List (genericReplicate, genericLength)

showIntegralWZeros :: Integral a => a -> a -> Maybe String
showIntegralWZeros strLen i =
  let i' = toInteger i
   in case compare strLen (genericLength (show i')) of
        LT -> Nothing
        EQ -> Just $ show i'
        GT -> let lengthDiff = strLen - genericLength (show i')
               in Just $ (genericReplicate lengthDiff '0') ++ (show i')
