{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Tests.Maybe (testMaybeModule) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isNothing)

import MyUtils.Maybe (justIf)
import MyUtils.Console

prop_justIfFalseAlwaysGivesNothing :: Eq a => a -> Bool
prop_justIfFalseAlwaysGivesNothing a =
  isNothing (justIf (const False) a)

prop_justIfTrueAlwaysGivesJust :: Eq a => a -> Bool
prop_justIfTrueAlwaysGivesJust a =
  justIf (const True) a == Just a

-- This line is necessary in order for the forAllProperties function to work
-- Something to do with template haskell
return []

testMaybeModule :: IO ()
testMaybeModule = do
  colorPutStrLn Red "\nTesting the MyUtils.Maybe module..."
  _ <- $quickCheckAll
  hspec $
    describe "justIf" $ do
      it "returns Just 'p' when given: (>'l') 'p'" $
        justIf (>'l') 'p' `shouldBe` Just 'p'
      it "returns Nothing when given: (>'l') 'b'" $
        justIf (>'l') 'b' `shouldBe` Nothing
