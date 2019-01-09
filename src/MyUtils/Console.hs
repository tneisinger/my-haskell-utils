{-|
Module      : MyUtils.Console
Description : Useful functions for console interaction
Copyright   : (c) Tyler Neisinger, 2018
License     : GPL-3
Maintainer  : tjneisi@gmail.com
Stability   : experimental
Portability : POSIX

This module contains functions that are useful when interacting with the
console.  For example, the colorPutStr function allows you to print text of any
color to the console by simply specifying a Color and providing a String.
-}
module MyUtils.Console
  ( module MyUtils.Console
  , Color (..)
  )
  where

import System.Console.ANSI

{-|
  Set the console text color to the given color, run the given function, and
  then reset the console color back to normal.  This function is meant to be
  used with functions print putStr or putStrLn.
-}
colorPut :: (String -> IO ()) -> Color -> String -> IO ()
colorPut f color str = do
  setSGR [SetColor Foreground Vivid color]
  f str
  setSGR [Reset]

{-|
  Specify a Color and give a String, and colorPutStr will print the given
  string in the specified color.
-}
colorPutStr :: Color -> String -> IO ()
colorPutStr = colorPut putStr

{-|
  This function is the same as colorPutStr, but uses putStrLn instead of
  putStr.
-}
colorPutStrLn :: Color -> String -> IO ()
colorPutStrLn = colorPut putStrLn

{-|
  Print the list of Colors, using each color to show what they look like.  This
  is great for when you aren't sure what color to use or what colors are
  available.
-}
printColors :: IO ()
printColors = do
  let colors = [Black ..]
      f c = putStr (show c ++ ": ") >> colorPutStrLn c (show c)
  mapM_ f colors

