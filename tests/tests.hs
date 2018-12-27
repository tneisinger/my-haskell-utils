{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)

import MyUtils.Read

prop_true :: Char -> Bool
prop_true _ = True

tester :: Gen String
tester = unpack <$> matchRegexp "(\\d|[a\\.]){1,3}"

main :: IO ()
main = do
  putStrLn "\n\nRUNNING QUICKCHECK...\n"
  quickCheck prop_true
  putStrLn "\n\nRUNNING HSPEC...\n"
  hspec $ do
    describe "maybeRead" $ do
      it "returns (Just 31) when given '31'" $ do
        (maybeRead "31" :: Maybe Integer) `shouldBe` (Just 31)
