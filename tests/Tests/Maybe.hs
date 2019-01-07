{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.Maybe (testMaybeModule) where

import Test.Hspec
import Test.QuickCheck

import MyUtils.Maybe (justIf)
import MyUtils.Console

prop_justIfFalseAlwaysGivesNothing :: Eq a => a -> Bool
prop_justIfFalseAlwaysGivesNothing a =
  justIf (const False) a == Nothing

prop_justIfTrueAlwaysGivesJust :: Eq a => a -> Bool
prop_justIfTrueAlwaysGivesJust a =
  justIf (const True) a == Just a

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testMaybeModule :: IO ()
testMaybeModule = do
  colorPutStrLn Red "\nTesting the MyUtils.Maybe module..."
  _ <- $forAllProperties quickCheckResult
  hspec $ do
    describe "justIf" $ do
      it "returns Just 'p' when given: (>'l') 'p'" $ do
        justIf (>'l') 'p' `shouldBe` Just 'p'
      it "returns Nothing when given: (>'l') 'b'" $ do
        justIf (>'l') 'b' `shouldBe` Nothing
