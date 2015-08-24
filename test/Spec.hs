
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Stack.Tag

main :: IO ()
main = hspec $ do
  describe "Stack.Tag" $ do
    it "sanity checks" $
        reverse [1,2,3,4] `shouldBe` [4,3,2,1]
    it "sanity checks quickcheck" $ property $
        \x -> (read . show) x == (x :: Int)
