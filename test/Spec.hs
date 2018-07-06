{-# LANGUAGE OverloadedStrings #-}
import           Data.Default

import           Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Data.Maybe


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [canonicalRequestTest]


targetRequest :: Maybe Request
targetRequest = do
    initReq <- parseUrlThrow "https://example.amazonaws.com"
    let req = initReq
                { requestHeaders =
                    [ ("Accept", "text/json")
                    , ("Content-Type", "application/x-www-form-urlencoded")
                    , ("X-Amz-Date", "20150830T123600Z")]
                }
    return req

-- GET /?Param2=value2&Param1=value1 HTTP/1.1
-- Host:example.amazonaws.com
-- X-Amz-Date:20150830T123600Z
canonicalRequestTest :: TestTree
canonicalRequestTest =
    testGroup "Canonical request test" [
        testCase "simple" $
            isJust targetRequest @?= True
    ]
