{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)

import MyUtils.Read
import MyUtils.Console

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
  colorPutStr Red "RUNNING HSPEC..."
  putStr "\n"
  hspec $ do
    describe "maybeRead" $ do
      it "returns (Just 31) when given '31'" $ do
        (maybeRead "31" :: Maybe Integer) `shouldBe` (Just 31)
