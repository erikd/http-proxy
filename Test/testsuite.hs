{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent.Async
import Data.Char (toUpper)
import Test.Hspec

-- import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Network.HTTP.Proxy

import Test.TestServer
import Test.Util

proxyTestDebug :: Bool
proxyTestDebug = True

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
    describe "Proxying HTTP:" $ proxyTest ("http", httpTestPort) debug
    describe "Proxying HTTPS:" $ proxyTest ("https", httpsTestPort) debug

proxyTest :: (String, Int) -> Bool -> Spec
proxyTest (trans, port) debug = do
    let tname = map toUpper trans
    it ("Simple " ++ tname ++ " GET.") $
        testSingleUrl debug ( HT.methodGet, trans ++ "://localhost:" ++ show port ++ "/", Nothing )
    it ("Simple " ++ tname ++ " GET with query.") $
        testSingleUrl debug ( HT.methodGet, trans ++ "://localhost:" ++ show port ++ "/a?b=1&c=2", Nothing )
    it ("Simple " ++ tname ++ " GET with request body.") $
        testSingleUrl debug ( HT.methodGet, trans ++ "://localhost:" ++ show port ++ "/", Just "Hello server!" )
    it ("Simple " ++ tname ++ " GET /forbidden returns 403.") $
        testSingleUrl debug ( HT.methodGet,  trans ++ "://localhost:" ++ show port ++ "/forbidden", Nothing )
    it ("Simple " ++ tname ++ " POST.") $
        testSingleUrl debug ( HT.methodPost, trans ++ "://localhost:" ++ show port ++ "/", Nothing )
    it ("Simple " ++ tname ++ " POST with request body.") $
        testSingleUrl debug ( HT.methodPost, trans ++ "://localhost:" ++ show port ++ "/", Just "Hello server!" )
    it ("Simple " ++ tname ++ " POST /forbidden returns 403.") $
        testSingleUrl debug ( HT.methodGet,  trans ++ "://localhost:" ++ show port ++ "/forbidden", Nothing )


runTestProxy :: Settings -> IO ()
runTestProxy settings = catchAny (runProxySettings settings) $ print

testProxySettings :: Settings
testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
