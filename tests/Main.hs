{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)

import Tests.Read
import Tests.List
import Tests.Maybe
import Tests.Show

main :: IO ()
main = do
  testReadModule
  testListModule
  testMaybeModule
  testShowModule
