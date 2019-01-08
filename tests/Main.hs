module Main where

import Tests.Read
import Tests.List
import Tests.Maybe
import Tests.Show
import Tests.Time

main :: IO ()
main = do
  testReadModule
  testListModule
  testMaybeModule
  testShowModule
  testTimeModule
