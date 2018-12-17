module MyUtils.Char where

import Data.Char

isDigitOrDot :: Char -> Bool
isDigitOrDot '.' = True
isDigitOrDot c = isDigit c
