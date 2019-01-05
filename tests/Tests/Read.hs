{-# LANGUAGE OverloadedStrings #-}

module Tests.Read
  ( testReadModule
  ) where

import Test.Hspec

import MyUtils.Read
import MyUtils.Console

testReadModule :: IO ()
testReadModule = do
  colorPutStrLn Red "Testing the Read module..."
  hspec $ do
    describe "maybeRead" $ do
      it "returns (Just 31) when given '31'" $ do
        (maybeRead "31" :: Maybe Integer) `shouldBe` (Just 31)
