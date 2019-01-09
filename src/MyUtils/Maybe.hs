{-|
Module      : MyUtils.Maybe
Description : Useful functions involving the Maybe type
Copyright   : (c) Tyler Neisinger, 2018
License     : GPL-3
Maintainer  : tjneisi@gmail.com
Stability   : experimental
Portability : POSIX

This module contains useful functions involving the Maybe type.
-}
module MyUtils.Maybe where

{-|
  This function abstracts out the common if-then-else pattern
  when working with Maybe values.  Given a predicate @p@ and
  some value of any type @v@, return @Just v@ if @p v@ is True,
  or @Nothing@ if @p v@ is False.

  Example:

    justIf (>4) 5
    Just 5

    >justIf (>4) 2
    Nothing
-}
justIf :: (a -> Bool) -> a -> Maybe a
justIf p v = if p v then Just v else Nothing
