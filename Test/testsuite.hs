{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Int (Int64)
import Test.Hspec

import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Network.HTTP.Proxy

import Test.TestServer
import Test.Util
import Test.Request
import Test.ServerDef


proxyTestDebug :: Bool
proxyTestDebug = False

main :: IO ()
main =
    withAsync (runTestServer httpTestPort) $ \ asyncHttpServer ->
    withAsync (runTestServerTLS httpsTestPort) $ \ asyncHttpsServer ->
    withAsync (runTestProxy testProxySettings) $ \ asyncProxy -> do
        hspec $ runProxyTests proxyTestDebug
        cancel asyncProxy
        cancel asyncHttpServer
        cancel asyncHttpsServer

runProxyTests :: Bool -> SpecWith ()
runProxyTests dbg = do
    describe "Test helper functionality:" testHelpersTest
    describe "Simple HTTP proxying:" $ proxyTest Http dbg
    describe "HTTP protocol:" protocolTest
    describe "Simple HTTPS proxying:" $ proxyTest Https dbg
    describe "HTTP streaming via proxy:" $ streamingTest dbg

proxyTest :: UriScheme -> Bool -> Spec
proxyTest uris dbg = do
    let tname = show uris
    it (tname ++ " GET.") $
        testSingleUrl dbg =<< mkGetRequest uris "/"
    it (tname ++ " GET with query.") $
        testSingleUrl dbg =<< mkGetRequest uris "/a?b=1&c=2"
    it (tname ++ " GET with request body.") $
        testSingleUrl dbg =<< mkGetRequestWithBody uris "/" "Hello server!"
    it (tname ++ " GET /forbidden returns 403.") $
        testSingleUrl dbg =<< mkGetRequest uris "/forbidden"
    it (tname ++ " GET /not-found returns 404.") $
        testSingleUrl dbg =<< mkGetRequest uris "/not-found"
    it (tname ++ " POST.") $
        testSingleUrl dbg =<< mkPostRequest uris "/"
    it (tname ++ " POST with request body.") $
        testSingleUrl dbg =<< mkPostRequestBS uris "/" "Hello server!"
    it (tname ++ " POST /forbidden returns 403.") $
        testSingleUrl dbg =<< mkPostRequest uris "/forbidden"
    it (tname ++ " POST /not-found returns 404.") $
        testSingleUrl dbg =<< mkPostRequest uris "/not-found"


protocolTest :: Spec
protocolTest =
    it ("Passes  re-directs through to client.") $ do
        req <- mkGetRequest Http "/301"
        Result _ status hdrs _ <- httpRun req
        status `shouldBe` 301
        lookup HT.hLocation hdrs `shouldBe` Just ("http://other-server/301")


testHelpersTest :: Spec
testHelpersTest = do
    it "Byte Sink catches short response bodies." $
        runResourceT (byteSource 80 $$ byteSink 100)
                `shouldReturn` Just "Error : Body length 80 should have been 100."
    it "Byte Source and Sink work in constant memory." $
        runResourceT (byteSource oneBillion $$ byteSink oneBillion) `shouldReturn` Nothing
    it "Byte Sink catches long response bodies." $
        runResourceT (byteSource 110 $$ byteSink 100)
                `shouldReturn` Just "Error : Body length 110 should have been 100."
    it "Client and server can stream GET response." $ do
        let size = oneBillion
            sizeStr = show size
        req <- mkGetRequest Http ("/large-get?" ++ sizeStr)
        Result _ status hdrs _ <- httpRun req
        status `shouldBe` 200
        lookup HT.hContentLength hdrs `shouldBe` Just (BS.pack sizeStr)
    it "Client and server can stream POST request." $ do
        let size = oneMillion
            sizeStr = show size
            body = HC.requestBodySourceIO size $ byteSource size
        req <- mkPostRequestBody Http ("/large-post?" ++ sizeStr) body
        Result _ status _ bs <- httpRun req
        status `shouldBe` 200
        bs `shouldBe` BS.pack ("Post-size: " ++ sizeStr)


-- Only need to do this test for HTTP.
streamingTest :: Bool -> Spec
streamingTest dbg = do
    forM_ [ 100, oneThousand, oneMillion, oneBillion ] $ \ size -> do
        it ("Http GET " ++ show (size :: Int64) ++ " bytes.") $
            testSingleUrl dbg =<< mkGetRequest Http ("/large-get?" ++ show size)
    forM_ [ 100, oneThousand, oneMillion, oneBillion ] $ \ size -> do
        it ("Http POST " ++ show (size :: Int64) ++ " bytes.") $ do
            let body = HC.requestBodySourceIO size $ byteSource size
            testSingleUrl dbg =<< mkPostRequestBody Http ("/large-post?" ++ show size) body


oneThousand, oneMillion, oneBillion :: Int64
oneThousand = 1000
oneMillion = oneThousand * oneThousand
oneBillion = oneThousand * oneMillion

runTestProxy :: Settings -> IO ()
runTestProxy settings = catchAny (runProxySettings settings) print

testProxySettings :: Settings
testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
