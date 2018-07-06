{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString           (ByteString)
import           Data.Maybe
import           Lib
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Test.Tasty
import           Test.Tasty.HUnit          (testCase, (@?=))


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [canonicalRequestTest]


targetRequestM :: Maybe Request
targetRequestM = do
    initReq <- parseUrlThrow "https://example.amazonaws.com"
    let req = setRequestQueryString [("Param2", Just "value2"), ("Param1", Just "value1")] initReq
                { requestHeaders =
                    [ ("X-Amz-Date", "20150830T123600Z")]
                }
    return req

targetRequest :: Request
targetRequest = fromJust targetRequestM


expected :: ByteString
expected = "GET\
\/\
\Param1=value1&Param2=value2\
\host:example.amazonaws.com\
\x-amz-date:20150830T123600Z\
\\
\host;x-amz-date\
\e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

-- GET /?Param2=value2&Param1=value1 HTTP/1.1
-- Host:example.amazonaws.com
-- X-Amz-Date:20150830T123600Z
canonicalRequestTest :: TestTree
canonicalRequestTest =
    testGroup "Canonical request test" [
        testCase "simple" $
        canonicalRequest targetRequest "" @?= expected

    ]
