{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.HttpHttpsRewriteTest
    ( httpToHttpsRewriteTest
    )
where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import Network.HTTP.Proxy

import Test.TestServer
import qualified Test.Util as U



debug :: Bool
debug = False


httpToHttpsRewriteTest :: IO ()
httpToHttpsRewriteTest = do
    U.printTestMsgR "Rewrite HTTP to HTTPS test"

    -- Don't need to do anything with these ThreadIds
    t1 <- forkIO $ runTestServerTLS U.httpTestPort
    t2 <- forkIO $ runProxySettings proxySettings
    mapM_ (testSingleUrl debug) tests
    U.printPassR
    killThread t1
    killThread t2
  where
    proxySettings = defaultProxySettings
                { proxyPort = U.testProxyPort
                , proxyHost = "*6"
                , proxyRequestModifier = Just httpsRedirector
                }
    tests =
        [ ( HT.methodGet,  "localhost", U.httpTestPort, "/", Nothing )
        , ( HT.methodPost, "localhost", U.httpTestPort, "/", Nothing )
        , ( HT.methodPost, "localhost", U.httpTestPort, "/", Just "Message\n" )
        , ( HT.methodGet,  "localhost", U.httpTestPort, "/forbidden", Nothing )
        ]
    httpsRedirector :: Request -> IO Request
    httpsRedirector _req = error "httpRedirector"
{-
     | serverName req == "localhost" && not (isSecure req) = do
             return $ req
                    { isSecure = True
                    , serverPort = U.httpTestPort
                    }

     | otherwise = return req
-}

--------------------------------------------------------------------------------
type TestRequest = ( HT.Method, String, Int, String, Maybe ByteString )


testSingleUrl :: Bool -> TestRequest -> IO ()
testSingleUrl dbg testreq = do
    (dreq, preq) <- liftIO $ setupRequest testreq
    when dbg $ liftIO $ do
        U.dumpHttpConduitRequest dreq
        U.dumpHttpConduitRequest preq
    direct <- U.httpRun dreq
    proxy <- U.httpRun $ HC.addProxy "localhost" U.testProxyPort preq
    when dbg $ liftIO $ do
        U.printResult direct
        U.printResult proxy
    U.compareResult direct proxy


setupRequest :: TestRequest -> IO (HC.Request, HC.Request)
setupRequest (method, host, port, path, reqBody) = do
    req <- HC.parseUrl $ "http://" ++ host ++ ":" ++ show port ++ path
    return
        ( req
            { HC.method = method
            , HC.secure = True
            , HC.port = if HC.port req == 443 then 443 else HC.port req
            , HC.requestBody = case reqBody of
                                Just x -> HC.RequestBodyBS x
                                Nothing -> HC.requestBody req
            -- In this test program we want to pass error pages back to the test
            -- function so the error output can be compared.
            , HC.checkStatus = \ _ _ _ -> Nothing
            }
        , req
            { HC.method = method
            , HC.secure = False
            , HC.port = if HC.port req == 80 then 80 else HC.port req
            , HC.requestBody = case reqBody of
                                Just x -> HC.RequestBodyBS x
                                Nothing -> HC.requestBody req
            -- In this test program we want to pass error pages back to the test
            -- function so the error output can be compared.
            , HC.checkStatus = \ _ _ _ -> Nothing
            }
        )

