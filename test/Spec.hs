{-# LANGUAGE OverloadedStrings #-}

import qualified   Data.ByteString as BS
import             Data.Maybe
import             Lib
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import             Test.Hspec


targetRequestM :: Maybe Request
targetRequestM = do
    initReq <- parseUrlThrow "https://iam.amazonaws.com"
    let req = setRequestQueryString [("Action", Just "ListUsers"), ("Version", Just "2010-05-08")] initReq
                { requestHeaders =
                    [ ("X-Amz-Date", "20150830T123600Z"),
                      ("Content-Type", "application/x-www-form-urlencoded; charset=utf-8")]
                }
    return req

targetRequest :: Request
targetRequest = fromJust targetRequestM

main :: IO ()
main = hspec $ do
  describe "canonicalRequest" $ do
    it "task 1" $ do
        expectedContent <- BS.readFile "data/example.txt"
        (canonicalRequest targetRequest "") `shouldBe` expectedContent
