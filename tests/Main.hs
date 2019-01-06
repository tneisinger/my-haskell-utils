{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)

import Tests.Read
import Tests.List

prop_true :: Char -> Bool
prop_true _ = True

tester :: Gen String
tester = unpack <$> matchRegexp "(\\d|[a\\.]){1,3}"

main :: IO ()
main = do
  -- quickCheck prop_true
  testReadModule
  testListModule
