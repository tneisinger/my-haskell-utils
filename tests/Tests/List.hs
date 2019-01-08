{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.List (testListModule) where

import Test.Hspec
import Test.QuickCheck

import MyUtils.List (trueFor, trueForZeroTo)
import MyUtils.Console

prop_trueFor :: Int -> [b] -> Bool
prop_trueFor n xs =
  trueFor n (const True) xs == (length xs == n)

prop_trueForZeroTo :: Int -> [b] -> Bool
prop_trueForZeroTo n xs =
  trueForZeroTo n (const True) xs == (length xs <= n)

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testListModule :: IO ()
testListModule = do
  colorPutStrLn Red "\nTesting the MyUtils.List module..."
  _ <- $quickCheckAll
  hspec $ do

    describe "trueFor" $ do
      it "returns True for inputs: 2 (>3) [1,4,6,2,3]" $
        trueFor 2 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns False for inputs: 3 (>3) [1,4,6,2,3]" $
        trueFor 3 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns False for inputs: 1 (>3) [1,4,6,2,3]" $
        trueFor 1 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"arcade\"" $
        trueFor 2 (=='a') "arcade" `shouldBe` True
      it "returns False for inputs: 2 (=='a') \"banana\"" $
        trueFor 2 (=='a') "banana" `shouldBe` False
      it "returns False for inputs: 2 (=='a') \"apple\"" $
        trueFor 2 (=='a') "apple" `shouldBe` False

    describe "trueForZeroTo" $ do
      it "returns True for inputs: 2 (>3) [1,4,6,2,3]" $
        trueForZeroTo 2 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns True for inputs: 3 (>3) [1,4,6,2,3]" $
        trueForZeroTo 3 (>3) [1,4,6,2,3] `shouldBe` True
      it "returns False for inputs: 1 (>3) [1,4,6,2,3]" $
        trueForZeroTo 1 (>3) [1,4,6,2,3] `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"arcade\"" $
        trueForZeroTo 2 (=='a') "arcade" `shouldBe` True
      it "returns False for inputs: 2 (=='a') \"banana\"" $
        trueForZeroTo 2 (=='a') "banana" `shouldBe` False
      it "returns True for inputs: 2 (=='a') \"apple\"" $
        trueForZeroTo 2 (=='a') "apple" `shouldBe` True
