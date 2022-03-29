{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Conduit
import Data.Int (Int64)
#if ! MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import System.Environment
import Test.Hspec

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai

import Network.HTTP.Proxy

import Test.TestServer
import Test.Util
import Test.Request
import Test.ServerDef


proxyTestDebug :: Bool
proxyTestDebug = False

main :: IO ()
main = do
    -- Clear the `http_proxy` enviroment variable.
    unsetEnv "http_proxy"
    bracket
        (mapM async [ runTestServer, runTestServerTLS ])
        (mapM_ cancel)
        (const $ runProxyTests proxyTestDebug)

runProxyTests :: Bool -> IO ()
runProxyTests dbg = hspec $ do
    testHelpersTest
    proxyTest Http dbg
    protocolTest
    proxyTest Https dbg
    streamingTest dbg
    requestTest

-- -----------------------------------------------------------------------------

testHelpersTest :: Spec
testHelpersTest =
    -- Test the HTTP and HTTPS servers directly (ie bypassing the Proxy).
    describe "Test helper functionality:" $ do
        it "Byte Sink catches short response bodies." $
            runConduit (byteSource 80 .| byteSink 100)
                `shouldReturn` Just "Error : Body length 80 should have been 100."
        it "Byte Source and Sink work in constant memory." $
            runConduit (byteSource oneBillion .| byteSink oneBillion) `shouldReturn` Nothing
        it "Byte Sink catches long response bodies." $
            runConduit (byteSource 110 .| byteSink 100)
                `shouldReturn` Just "Error : Body length 110 should have been 100."
        it "Client and server can stream GET response." $ do
            let size = oneBillion
                sizeStr = show size
            result <- httpRun =<< mkGetRequest Http ("/large-get?" ++ sizeStr)
            resultStatus result `shouldBe` 200
            lookup HT.hContentLength (resultHeaders result) `shouldBe` Just (BS.pack sizeStr)
        it "Client and server can stream POST request." $ do
            let size = oneMillion
                sizeStr = show size
                body = HC.requestBodySourceIO size $ byteSource size
            result <- httpRun =<< mkPostRequestBody Http ("/large-post?" ++ sizeStr) body
            resultStatus result `shouldBe` 200
            resultBS result `shouldBe` BS.pack ("Post-size: " ++ sizeStr)


proxyTest :: UriScheme -> Bool -> Spec
proxyTest uris dbg = around withDefaultTestProxy $
    describe ("Simple " ++ show uris ++ " proxying:") $ do
        let tname = show uris
        it (tname ++ " GET.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkGetRequest uris "/"
        it (tname ++ " GET with query.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkGetRequest uris "/a?b=1&c=2"
        it (tname ++ " GET with request body.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkGetRequestWithBody uris "/" "Hello server!"
        it (tname ++ " GET /forbidden returns 403.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkGetRequest uris "/forbidden"
        it (tname ++ " GET /not-found returns 404.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkGetRequest uris "/not-found"
        it (tname ++ " POST.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkPostRequest uris "/"
        it (tname ++ " POST with request body.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkPostRequestBS uris "/" "Hello server!"
        it (tname ++ " POST /forbidden returns 403.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkPostRequest uris "/forbidden"
        it (tname ++ " POST /not-found returns 404.") $ \ testProxyPort ->
            testSingleUrl dbg testProxyPort =<< mkPostRequest uris "/not-found"


protocolTest :: Spec
protocolTest = around withDefaultTestProxy $
    describe "HTTP protocol:" $
        it "Passes re-directs through to client." $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/301"
            result <- httpRun req
            resultStatus result `shouldBe` 301
            lookup HT.hLocation (resultHeaders result) `shouldBe` Just "http://other-server/301"


-- Only need to do this test for HTTP not HTTPS (because it just streams bytes
-- back and forth).
streamingTest :: Bool -> Spec
streamingTest dbg = around withDefaultTestProxy $
    describe "HTTP streaming via proxy:" $ do
        forM_ [ 100, oneThousand, oneMillion, oneBillion ] $ \ size ->
            it ("Http GET " ++ show (size :: Int64) ++ " bytes.") $ \ testProxyPort ->
                testSingleUrl dbg testProxyPort =<< mkGetRequest Http ("/large-get?" ++ show size)
        forM_ [ 100, oneThousand, oneMillion, oneBillion ] $ \ size ->
            it ("Http POST " ++ show (size :: Int64) ++ " bytes.") $ \ testProxyPort -> do
                let body = HC.requestBodySourceIO size $ byteSource size
                testSingleUrl dbg testProxyPort =<< mkPostRequestBody Http ("/large-post?" ++ show size) body


-- Test that a Request can be pulled apart and reconstructed without losing
-- anything.
requestTest :: Spec
requestTest = describe "Request:" $ do
    it "Can add a request header." $
        withTestProxy proxySettingsAddHeader $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/whatever"
            result <- httpRun req
            "X-Test-Header: Blah" `BS.isInfixOf` resultBS result `shouldBe` True
    it "Can add a response header." $
        withTestProxy proxySettingsAddResponseHeader $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/whatever"
            result <- httpRun req
            ("X-Test-Header", "Blah") `elem` resultHeaders result `shouldBe` True
    it "Can retry a request." $
        withTestProxy proxySettingsRetry $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/whatever"
            result <- httpRun req
            resultBS result `shouldBe` "This is another page"
    it "Can rewrite HTTP to HTTPS." $
        withTestProxy proxySettingsHttpsUpgrade $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/secure"
            result <- httpRun req
            -- Getting a TlsException shows that we have successfully upgraded
            -- from HTTP to HTTPS. Its not possible to ignore this failure
            -- because its made by the http-conduit inside the proxy.
            BS.takeWhile (/= ' ') (resultBS result) `shouldBe` "HttpExceptionRequest"
    it "Can provide a proxy Response." $
        withTestProxy proxySettingsProxyResponse $ \ testProxyPort -> do
            req <- addTestProxy testProxyPort <$> mkGetRequest Http "/whatever"
            result <- httpRun req
            resultBS result `shouldBe` "This is the proxy reqponse"

-- -----------------------------------------------------------------------------

oneThousand, oneMillion, oneBillion :: Int64
oneThousand = 1000
oneMillion = oneThousand * oneThousand
oneBillion = oneThousand * oneMillion


withDefaultTestProxy :: (Int -> IO ()) -> IO ()
withDefaultTestProxy action = do
    (sock, portnum) <- openLocalhostListenSocket
    bracket (async $ runProxySettingsSocket defaultProxySettings sock) cancel (const $ action portnum)


withTestProxy :: Settings -> (Int -> Expectation) -> Expectation
withTestProxy settings expectation = do
    (sock, portnum) <- openLocalhostListenSocket
    bracket (async $ runProxySettingsSocket settings sock) cancel (const $ expectation portnum)


proxySettingsAddHeader :: Settings
proxySettingsAddHeader = defaultProxySettings
    { proxyHttpRequestModifier = \ req -> return . Right $ req
                { requestHeaders = (CI.mk "X-Test-Header", "Blah") : requestHeaders req
                }
    }

proxySettingsAddResponseHeader :: Settings
proxySettingsAddResponseHeader = defaultProxySettings
    { proxyHttpResponseModifier = \ _ resp -> return $ Wai.mapResponseHeaders ((CI.mk "X-Test-Header", "Blah") :) resp
    }

proxySettingsRetry :: Settings
proxySettingsRetry = defaultProxySettings
    { proxyHttpResponseModifier = \ req _ ->
      if BS.isSuffixOf "/another" $ requestPath req
      then return $ simpleResponse HT.status200 "This is another page"
      else retryWithRequest $ req { requestPath = redirect $ requestPath req }
    }
  where
    redirect path = fst (BS.spanEnd (/= '/') path) `BS.append` "another"

proxySettingsHttpsUpgrade :: Settings
proxySettingsHttpsUpgrade = defaultProxySettings
    { proxyHttpRequestModifier = \ req -> return . Right $ req { requestPath = httpsUpgrade $ requestPath req }
    }
  where
    httpsUpgrade bs =
        let (start, end) = BS.breakSubstring (bsShow $ httpTestPort portsDef) bs
            https = bsShow $ httpsTestPort portsDef
        in "https" <> BS.drop 4 start <> https <> BS.drop 5 end
    bsShow = BS.pack . show

proxySettingsProxyResponse :: Settings
proxySettingsProxyResponse = defaultProxySettings
    { proxyHttpRequestModifier = const . return $ Left proxyResponse
    }
  where
    proxyResponse :: Wai.Response
    proxyResponse = simpleResponse HT.status200 "This is the proxy reqponse"
