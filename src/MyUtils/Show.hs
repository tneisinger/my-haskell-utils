{-|
Module      : MyUtils.Show
Description : Useful functions for showing different values
Copyright   : (c) Tyler Neisinger, 2018
License     : GPL-3
Maintainer  : tjneisi@gmail.com
Stability   : experimental
Portability : POSIX

This module contains useful functions for converting different types into
Strings in different ways.
-}
module MyUtils.Show where

import Text.Printf (printf)
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
         then dashOrEmpty ++ genericReplicate numZeros '0' ++ show (abs i')
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

{-|
  This function will return a String representation of a Double value rounded
  to n or fewer decimal places.  After the tenths place, zeros will be cropped.

  Examples:

    > showDoubleRoundedTo 3 23.40000001
    "23.4"

    > showDoubleRoundedTo 3 23.45879
    "23.459"

    > showDoubleRoundedTo 5 23.000
    "23.0"
-}
showDoubleRoundedTo :: Int -> Double -> String
showDoubleRoundedTo n d =
  let trimmed = dropWhile (=='0') $ reverse $ printf ("%." ++ show n ++ "f") d
   -- In the line above we removed all trailing zeros, but we want to put one
   -- back if we removed the tenths-place zero.
   in case trimmed of
        ('.':_) -> reverse $ '0':trimmed
        _       -> reverse trimmed
