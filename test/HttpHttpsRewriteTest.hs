{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module HttpHttpsRewriteTest
    ( httpToHttpsRewriteTest
    )
where

import Control.Monad.Trans.Resource
import Network.HTTP.Proxy

import Control.Concurrent (forkIO, killThread)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import TestServer
import qualified Util as U


debug :: Bool
debug = False


httpToHttpsRewriteTest :: IO ()
httpToHttpsRewriteTest = runResourceT $ do
    U.printTestMsgR "Rewrite HTTP to HTTPS test"

    -- Don't need to do anything with these ThreadIds
    _ <- allocate (forkIO $ runTestServerTLS U.testServerPort) killThread
    _ <- allocate (forkIO $ runProxySettings proxySettings) killThread
    mapM_ (testSingleUrl debug) tests
    U.printPassR
  where
    proxySettings = defaultSettings
                { proxyPort = U.testProxyPort
                , proxyHost = "*6"
                , proxyRequestModifier = httpsRedirector
                }
    tests =
        [ ( HT.methodGet,  "localhost", U.testServerPort, "/", Nothing )
        , ( HT.methodPost, "localhost", U.testServerPort, "/", Nothing )
        , ( HT.methodPost, "localhost", U.testServerPort, "/", Just "Message\n" )
        , ( HT.methodGet,  "localhost", U.testServerPort, "/forbidden", Nothing )
        ]
    httpsRedirector :: Request -> IO Request
    httpsRedirector req
     | serverName req == "localhost" && not (isSecure req) = do
             return $ req
                    { isSecure = True
                    , serverPort = U.testServerPort
                    }

     | otherwise = return req


--------------------------------------------------------------------------------
type TestRequest = ( HT.Method, String, Int, String, Maybe ByteString )


testSingleUrl :: Bool -> TestRequest -> ResourceT IO ()
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


setupRequest :: Monad m => TestRequest -> IO (HC.Request m, HC.Request m)
setupRequest (method, host, port, path, reqBody) = do
    req <- HC.parseUrl $ "http://" ++ host ++ ":" ++ show port ++ path
    return $
        ( req
            { HC.method = method
            , HC.secure = True
            , HC.port = if HC.port req == 443 then 443 else HC.port req
            , HC.requestBody = case reqBody of
                                Just x -> HC.RequestBodyBS x
                                Nothing -> HC.requestBody req
            -- In this test program we want to pass error pages back to the test
            -- function so the error output can be compared.
            , HC.checkStatus = \ _ _ -> Nothing
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
            , HC.checkStatus = \ _ _ -> Nothing
            }
        )

