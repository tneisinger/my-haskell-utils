{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.List where

import Test.Hspec

import MyUtils.List (trueFor, trueForZeroTo)
import MyUtils.Console

testListModule :: IO ()
testListModule = do
  colorPutStrLn Red "Testing the List module..."
  hspec $ do

    describe "trueFor" $ do
      it "returns True for inputs: 2 (>3) [1,4,6,2,3]" $ do
        trueFor 2 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns False for inputs: 3 (>3) [1,4,6,2,3]" $ do
        trueFor 3 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns False for inputs: 1 (>3) [1,4,6,2,3]" $ do
        trueFor 1 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"arcade\"" $ do
        trueFor 2 (=='a') "arcade" `shouldBe` True
      it "returns False for inputs: 2 (=='a') \"banana\"" $ do
        trueFor 2 (=='a') "banana" `shouldBe` False
      it "returns False for inputs: 2 (=='a') \"apple\"" $ do
        trueFor 2 (=='a') "apple" `shouldBe` False

    describe "trueForZeroTo" $ do
      it "returns True for inputs: 2 (>3) [1,4,6,2,3]" $ do
        trueForZeroTo 2 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns True for inputs: 3 (>3) [1,4,6,2,3]" $ do
        trueForZeroTo 3 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns False for inputs: 1 (>3) [1,4,6,2,3]" $ do
        trueForZeroTo 1 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"arcade\"" $ do
        trueForZeroTo 2 (=='a') "arcade" `shouldBe` True
      it "returns False for inputs: 2 (=='a') \"banana\"" $ do
        trueForZeroTo 2 (=='a') "banana" `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"apple\"" $ do
        trueForZeroTo 2 (=='a') "apple" `shouldBe` True
