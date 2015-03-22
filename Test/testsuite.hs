{-# LANGUAGE OverloadedStrings #-}


import Data.ByteString (ByteString)
import Control.Concurrent.Async
import Data.Char
import Test.Hspec

-- import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Network.HTTP.Proxy

import Test.TestServer
import Test.Util

proxyTestDebug :: Bool
proxyTestDebug = False

main :: IO ()
main =
    withAsync (runTestServer httpTestPort) $ \ asyncHttpServer ->
    withAsync (runTestServerTLS httpsTestPort) $ \ asyncHttpsServer ->
    withAsync (runTestProxy testProxySettings) $ \asyncProxy -> do
        hspec $ runProxyTests proxyTestDebug
        cancel asyncProxy
        cancel asyncHttpServer
        cancel asyncHttpsServer

runProxyTests :: Bool -> SpecWith ()
runProxyTests debug = do
    describe "Simple HTTP proxying:" $ proxyTest Http debug
    describe "Simple HTTPS proxying:" $ proxyTest Https debug

proxyTest :: UriScheme -> Bool -> Spec
proxyTest uris debug = do
    let tname = show uris
    it (tname ++ " GET.") $
        testSingleUrl debug $ mkGetRequest uris "/"
    it (tname ++ " GET with query.") $
        testSingleUrl debug $ mkGetRequest uris "/a?b=1&c=2"
    it (tname ++ " GET with request body.") $
        testSingleUrl debug $ mkGetRequestWithBody uris "/" "Hello server!"
    it (tname ++ " GET /forbidden returns 403.") $
        testSingleUrl debug $ mkGetRequest uris "/forbidden"
    it (tname ++ " GET /not-found returns 404.") $
        testSingleUrl debug $ mkGetRequest uris "/not-found"
    it (tname ++ " POST.") $
        testSingleUrl debug $ mkPostRequest uris "/"
    it (tname ++ " POST with request body.") $
        testSingleUrl debug $ mkPostRequestWithBody uris "/" "Hello server!"
    it (tname ++ " POST /forbidden returns 403.") $
        testSingleUrl debug $ mkPostRequest uris "/forbidden"
    it (tname ++ " POST /not-found returns 404.") $
        testSingleUrl debug $ mkPostRequest uris "/not-found"


mkTestRequest :: HT.Method -> UriScheme -> String -> Maybe ByteString -> TestRequest
mkTestRequest meth scheme path body =
    let port = show $ case scheme of
                        Http -> httpTestPort
                        Https -> httpsTestPort
    in  ( meth
        , map toLower (show scheme) ++ "://localhost:" ++ port ++ path
        , body
        )

mkGetRequest :: UriScheme -> String -> TestRequest
mkGetRequest scheme path = mkTestRequest get scheme path Nothing

mkGetRequestWithBody :: UriScheme -> String -> ByteString -> TestRequest
mkGetRequestWithBody scheme path body = mkTestRequest get scheme path (Just body)

mkPostRequest :: UriScheme -> String -> TestRequest
mkPostRequest scheme path = mkTestRequest post scheme path Nothing

mkPostRequestWithBody :: UriScheme -> String -> ByteString -> TestRequest
mkPostRequestWithBody scheme path body = mkTestRequest post scheme path (Just body)


runTestProxy :: Settings -> IO ()
runTestProxy settings = catchAny (runProxySettings settings) print

testProxySettings :: Settings
testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
