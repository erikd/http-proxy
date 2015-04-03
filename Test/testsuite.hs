{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

import Control.Concurrent.Async
import Control.Monad
import Data.Int (Int64)
import Test.Hspec

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
    describe "Simple HTTP proxying:" $ proxyTest Http dbg
    describe "Simple HTTPS proxying:" $ proxyTest Https dbg
    describe "HTTP streaming:" $ streamingTest dbg

proxyTest :: UriScheme -> Bool -> Spec
proxyTest uris dbg = do
    let tname = show uris
    it (tname ++ " GET.") $
        testSingleUrl dbg $ mkGetRequest uris "/"
    it (tname ++ " GET with query.") $
        testSingleUrl dbg $ mkGetRequest uris "/a?b=1&c=2"
    it (tname ++ " GET with request body.") $
        testSingleUrl dbg $ mkGetRequestWithBody uris "/" "Hello server!"
    it (tname ++ " GET /forbidden returns 403.") $
        testSingleUrl dbg $ mkGetRequest uris "/forbidden"
    it (tname ++ " GET /not-found returns 404.") $
        testSingleUrl dbg $ mkGetRequest uris "/not-found"
    it (tname ++ " POST.") $
        testSingleUrl dbg $ mkPostRequest uris "/"
    it (tname ++ " POST with request body.") $
        testSingleUrl dbg $ mkPostRequestWithBody uris "/" "Hello server!"
    it (tname ++ " POST /forbidden returns 403.") $
        testSingleUrl dbg $ mkPostRequest uris "/forbidden"
    it (tname ++ " POST /not-found returns 404.") $
        testSingleUrl dbg $ mkPostRequest uris "/not-found"


-- Only need to do this test for HTTP.
streamingTest :: Bool -> Spec
streamingTest dbg =
    forM_ [ 100, oneThousand, oneMillion, oneBillion ] $ \ size ->
        it ("Http GET " ++ show (size :: Int64) ++ " bytes.") $
            testSingleUrl dbg $ mkGetRequest Http ("/large-get?" ++ show size)


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
