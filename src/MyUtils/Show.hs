module MyUtils.Show where

import Data.List (genericReplicate, genericLength)
import MyUtils.Maybe (justIf)

{- |
  Given the minimum desired string length (strLen) and an integral (i), return
  a string with length equal to or greater than strLen.  Leading zeros will be
  added to the strieg so that the output string will have a length of at least
  strLen.

  Examples:

    > showIntegralWZeros 2 7
    "07"

    > showIntegralWZeros 3 12345
    "12345"

    > showIntegralWZeros 3 (-2)
    "-002"
-}
showIntegralWZeros :: Integral a => a -> a -> String
showIntegralWZeros strLen i =
  let i' = toInteger i
      numZeros = strLen - genericLength (show $ abs i')
      dashOrEmpty = if i' < 0 then "-" else ""
   in if numZeros > 0
         then dashOrEmpty ++ (genericReplicate numZeros '0') ++ (show $ abs i')
         else show i'

{- |
  Given the desired string length (strLen) and an integral (i), return i as a
  string of exactly length strLen.  Leading zeros will be added to the string
  so that the output string will have a length of exactly strLen.  If the
  string representation of the integral i would be longer than strLen, return
  Nothing.

  Examples:

    > maybeShowIntegralWZeros 2 7
    Just "07"

    > maybeShowIntegralWZeros 3 12345
    Nothing

    > maybeShowIntegralWZeros 3 (-2)
    Just "-002"
-}
maybeShowIntegralWZeros :: Integral a => a -> a -> Maybe String
maybeShowIntegralWZeros strLen i =
  let result = showIntegralWZeros strLen i
   in if i < 0
         then justIf (\r -> genericLength r == strLen + 1) result
         else justIf (\r -> genericLength r == strLen) result
