module MyUtils.Console
  ( module MyUtils.Console
  , Color (..)
  )
  where

import System.Console.ANSI

colorPut :: (String -> IO ()) -> Color -> String -> IO ()
colorPut f color str = do
  setSGR [SetColor Foreground Vivid color]
  f str
  setSGR [Reset]

colorPutStr :: Color -> String -> IO ()
colorPutStr = colorPut putStr

colorPutStrLn :: Color -> String -> IO ()
colorPutStrLn = colorPut putStrLn

printColors :: IO ()
printColors = do
  let colors = [Black ..]
      f c = putStr (show c ++ ": ") >> colorPutStrLn c (show c)
  mapM_ f colors

