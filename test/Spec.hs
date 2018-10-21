{-# LANGUAGE OverloadedStrings #-}

import qualified   Data.ByteString as BS
import             Data.Maybe
import             Lib
import             Test.Hspec



main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "read content" $ do
        (BS.readFile "data/example.txt") `shouldReturn`  ""
