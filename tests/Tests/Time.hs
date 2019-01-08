{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.Time (testTimeModule) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import Data.Time.Calendar (toGregorian)
import Data.List (intercalate)

import MyUtils.Time
import MyUtils.Show (showIntegralWZeros)
import MyUtils.Console

getDateTuple :: Num a => IO (a, a, a)
getDateTuple = do
  (y, m, d) <- toGregorian . utctDay <$> getCurrentTime
  return (fromIntegral y, fromIntegral m, fromIntegral d)

getTodayStrParts :: IO [String]
getTodayStrParts = do
  (y, m, d) <- getDateTuple
  let yStr = show y
      (mStr, dStr) = (showIntegralWZeros 2 m, showIntegralWZeros 2 d)
  return [yStr, mStr, dStr]

prop_getDateFromUTCTime_works :: UTCTime -> Bool
prop_getDateFromUTCTime_works t =
  let result = getDateFromUTCTime t
      (y, m, d) = toGregorian $ utctDay t
   in result == (y, fromIntegral m, fromIntegral d)

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testTimeModule :: IO ()
testTimeModule = do
  colorPutStrLn Red "\nTesting the MyUtils.Time module..."
  _ <- $quickCheckAll
  hspec $
    describe "getTodayStr" $ do
      it "returns today's date in YYYY-MM-DD format \
        \when given: YYYYMMDD \"-\"" $ do
          result <- getTodayStr YYYYMMDD "-"
          parts <- getTodayStrParts
          result `shouldBe` intercalate "-" parts
      it "returns today's date in YY/MM/DD format \
      \when given: YYMMDD \"/\"" $ do
          result <- getTodayStr YYMMDD "/"
          [y, m, d] <- getTodayStrParts
          result `shouldBe` intercalate "/" [drop 2 y, m, d]
      it "returns today's date in YYDDMM format \
      \when given: YYDDMM \"\"" $ do
          result <- getTodayStr YYDDMM ""
          [y, m, d] <- getTodayStrParts
          result `shouldBe` mconcat [drop 2 y, d, m]
      it "returns today's date in YYYY.DD.MM format \
      \when given: YYYYDDMM \".\"" $ do
          result <- getTodayStr YYYYDDMM "."
          [y, m, d] <- getTodayStrParts
          result `shouldBe` intercalate "." [y, d, m]
