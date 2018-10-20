{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString           (ByteString)
import           Data.Maybe
import           Lib
import Test.Hspec



main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
