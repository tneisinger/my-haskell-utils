{-# LANGUAGE OverloadedStrings #-}

module Main where

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
