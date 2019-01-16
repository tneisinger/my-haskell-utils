{-|
Module      : MyUtils.Read
Description : Useful functions for reading to text into haskell types
Copyright   : (c) Tyler Neisinger, 2018
License     : GPL-3
Maintainer  : tjneisi@gmail.com
Stability   : experimental
Portability : POSIX

This module contains useful functions for reading strings into haskell types.
-}
module MyUtils.Read where

{-|
  Given a String, maybe return a value of type a.  The type you wish to read
  must be specified somewhere in your program.
-}
maybeRead :: Read a => String -> Maybe a
maybeRead str =
  case reads str of
    [(a,[])] -> Just a
    _ -> Nothing

{-|
  Given an errMsg string and a string to read, return `Left errMsg` if the read
  was unsuccessful.  Return `Right a` if the read succeeded.
-}
eitherRead :: Read a => String -> String -> Either String a
eitherRead errMsg str =
  case maybeRead str of
    Nothing -> Left errMsg
    Just a  -> Right a
