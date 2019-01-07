{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Read
  ( testReadModule
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.StringRandom (matchRegexp)
import Data.Text (unpack)
import Data.Char (isDigit)

import MyUtils.Read
import MyUtils.Console

perhapsReadableAsInt :: Gen String
perhapsReadableAsInt = unpack <$> matchRegexp "(\\d|[a]){1,2}"

prop_maybeRead_canReadInts :: Property
prop_maybeRead_canReadInts =
  forAll perhapsReadableAsInt preprop_maybeRead_canReadAllDigits

preprop_maybeRead_canReadAllDigits :: String -> Bool
preprop_maybeRead_canReadAllDigits str =
  if all isDigit str
     then maybeRead str == Just (read str :: Int)
     else maybeRead str == (Nothing :: Maybe Int)

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testReadModule :: IO ()
testReadModule = do
  colorPutStrLn Red "\nTesting the Read module..."
  _ <- $quickCheckAll
  hspec $ do
    describe "maybeRead" $ do
      it "returns (Just 31) when given '31' and typed as Integer" $ do
        (maybeRead "31" :: Maybe Integer) `shouldBe` Just 31
      it "returns Nothing when given 'blarf' and typed as Integer" $ do
        (maybeRead "blarf" :: Maybe Integer) `shouldBe` Nothing
      it "returns (Just 31) when given '31' and typed as Int" $ do
        (maybeRead "31" :: Maybe Int) `shouldBe` Just 31
      it "returns Nothing when given '31' and typed as Bool" $ do
        (maybeRead "31" :: Maybe Bool) `shouldBe` Nothing
      it "returns (Just 0.37) when given '0.37' and typed as Double" $ do
        (maybeRead "0.37" :: Maybe Double) `shouldBe` Just 0.37
      it "returns Nothing when given '0.37' and typed as Integer" $ do
        (maybeRead "0.37" :: Maybe Integer) `shouldBe` Nothing
      it "returns (Just True) when given 'True' and typed as Bool" $ do
        (maybeRead "True" :: Maybe Bool) `shouldBe` Just True
      it "returns (Just False) when given 'False' and typed as Bool" $ do
        (maybeRead "False" :: Maybe Bool) `shouldBe` Just False
      it "returns (Just GT) when given 'GT' and typed as Ordering" $ do
        (maybeRead "GT" :: Maybe Ordering) `shouldBe` Just GT
      it "returns (Just (Right 2)) when given 'Right 2' typed as \
          \Either String Int" $ do
        (maybeRead "Right 2" :: Maybe (Either String Int)) `shouldBe`
          Just (Right 2)
      it "returns (Just [GT, EQ]) when given '[GT, EQ]' \
          \and typed as [Ordering]" $ do
        (maybeRead "[GT, EQ]" :: Maybe [Ordering]) `shouldBe` Just [GT, EQ]
