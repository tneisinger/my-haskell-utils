{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.Show (testShowModule) where

import Test.Hspec
import Test.QuickCheck

import MyUtils.Show
import MyUtils.Console

prop_showIntegralWZeros_outputLength :: Int -> Int -> Bool
prop_showIntegralWZeros_outputLength strLen i =
  let outputLength = length $ showIntegralWZeros strLen i
   in if length (show i) > strLen
        then outputLength == length (show i)
        else if i >= 0
               then outputLength == strLen
               else outputLength == strLen + 1

prop_showIntegralWZeros_containsVal :: Int -> Int -> Bool
prop_showIntegralWZeros_containsVal strLen i =
  let result = showIntegralWZeros strLen i
      minOutputLen = max 1 strLen
   in case compare i 0 of
        LT -> tail (show i) == dropWhile (=='0') (tail result)
        EQ -> replicate minOutputLen '0' == result
        GT -> show i == dropWhile (=='0') result

prop_maybeShowIntegralWZeros_outputLength :: Int -> Int -> Bool
prop_maybeShowIntegralWZeros_outputLength strLen i =
  case (maybeShowIntegralWZeros strLen i, i < 0) of
    (Just str, True)  -> length str == strLen + 1
    (Just str, False) -> length str == strLen
    (Nothing, True)   -> length (show i) > strLen + 1
    (Nothing, False)  -> length (show i) > strLen

prop_maybeShowIntegralWZeros_containsVal :: Int -> Int -> Bool
prop_maybeShowIntegralWZeros_containsVal strLen i =
  case (maybeShowIntegralWZeros strLen i, compare i 0) of
    (Just str, LT)  -> show (abs i) == dropWhile (=='0') (tail str)
    (Just str, EQ)  -> length str == strLen && all (=='0') str
    (Just str, GT)  -> show i == dropWhile (=='0') str
    _               -> length (show (abs i)) > strLen

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testShowModule :: IO ()
testShowModule = do
  colorPutStrLn Red "\nTesting the MyUtils.Show module..."
  _ <- $forAllProperties quickCheckResult
  hspec $ do
    describe "showIntegralWZeros" $ do
      it "returns \"07\" when given: 2 7" $
        showIntegralWZeros 2 7 `shouldBe` "07"
      it "returns \"12345\" when given: 3 12345" $
        showIntegralWZeros 3 12345 `shouldBe` "12345"
      it "returns \"-002\" when given: 3 (-2)" $
        showIntegralWZeros 3 (-2) `shouldBe` "-002"

    describe "maybeShowIntegralWZeros" $ do
      it "returns Just \"07\" when given: 2 7" $
        maybeShowIntegralWZeros 2 7 `shouldBe` Just "07"
      it "returns Nothing when given: 3 12345" $
        maybeShowIntegralWZeros 3 12345 `shouldBe` Nothing
      it "returns Just \"-002\" when given: 3 (-2)" $
        maybeShowIntegralWZeros 3 (-2) `shouldBe` Just "-002"

