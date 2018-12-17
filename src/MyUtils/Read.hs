module MyUtils.Read where

import Data.Char (isDigit)

import MyUtils.Char (isDigitOrDot)
import MyUtils.Maybe (maybeIf)
import MyUtils.List (justN)

isReadableInteger :: String -> Bool
isReadableInteger [] = False
isReadableInteger ('-':xs) = all isDigit xs
isReadableInteger xs = all isDigit xs

isReadableFloat :: String -> Bool
isReadableFloat [] = False
isReadableFloat (x:[]) = isDigit x
isReadableFloat ('-':x:xs) =
  isDigit x && all isDigitOrDot xs && justN 1 (== '.') xs && isDigit (last xs)
isReadableFloat (x:xs) =
  isDigit x && all isDigitOrDot xs && justN 1 (== '.') xs && isDigit (last xs)

readInteger :: String -> Maybe Integer
readInteger str = maybeIf (isReadableInteger str) (read str)

readDouble :: String -> Maybe Double
readDouble str = maybeIf (isReadableFloat str) (read str)
