module MyUtils.Read where

import Data.Char (isDigit)

import MyUtils.Char (isDigitOrDot)
import MyUtils.Maybe (justIf)
import MyUtils.List (trueForZeroTo)

isReadableInteger :: String -> Bool
isReadableInteger [] = False
isReadableInteger ('-':xs) = all isDigit xs
isReadableInteger xs = all isDigit xs

isReadableFloat :: String -> Bool
isReadableFloat [] = False
isReadableFloat (x:[]) = isDigit x
isReadableFloat ('-':x:[]) = isDigit x
isReadableFloat ('-':x:xs) = isDigit x && all isDigitOrDot xs &&
  trueForZeroTo 1 (== '.') xs && isDigit (last xs)
isReadableFloat (x:xs) = isDigit x && all isDigitOrDot xs &&
  trueForZeroTo 1 (== '.') xs && isDigit (last xs)

readInteger :: String -> Maybe Integer
readInteger str = justIf (isReadableInteger str) (read str)

readDouble :: String -> Maybe Double
readDouble str = justIf (isReadableFloat str) (read str)
