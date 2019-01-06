{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)

import MyUtils.Console
import Tests.Read
import Tests.List

prop_true :: Char -> Bool
prop_true _ = True

tester :: Gen String
tester = unpack <$> matchRegexp "(\\d|[a\\.]){1,3}"

main :: IO ()
main = do
  putStr "\n\n"
  colorPutStr Red "RUNNING QUICKCHECK..."
  putStr "\n\n"
  quickCheck prop_true
  putStr "\n"
  testReadModule
  testListModule
