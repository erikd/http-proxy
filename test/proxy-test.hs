{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

import Blaze.ByteString.Builder
import Control.Monad.Trans.Resource
import Network.HTTP.Proxy

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Char (isSpace)
import Data.Conduit (($$))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit as DC
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import TestServer
import HttpHttpsRewriteTest
import Util


hugeLen :: Int64
hugeLen = 8 * 1000 * 1000 * 1000


debug :: Bool
debug = False


main :: IO ()
main = do
    basicTest
    warpTlsTest
    httpsConnectTest
    httpToHttpsRewriteTest
    streamingTest


basicTest :: IO ()
basicTest = runResourceT $ do
    printTestMsgR "Basic tests"
    -- Don't need to do anything with these ThreadIds
    _ <- allocate (forkIO $ runTestServer testServerPort) killThread
    _ <- allocate (forkIO $ runProxySettings testProxySettings) killThread
    mapM_ (testSingleUrl debug) tests
    printPassR
  where
    testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
    tests =
        [ ( HT.methodGet,  "http://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "http://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "http://localhost:" ++ show testServerPort ++ "/", Just "Message\n" )
        , ( HT.methodGet,  "http://localhost:" ++ show testServerPort ++ "/forbidden", Nothing )
        ]

--------------------------------------------------------------------------------

streamingTest :: IO ()
streamingTest = runResourceT $ do
    -- Don't need to do anything with these ThreadIds
    _ <- allocate (forkIO $ runTestServer testServerPort) killThread
    _ <- allocate (forkIO $ runProxySettings testProxySettings) killThread
    streamingGetTest  1000 $ "http://localhost:" ++ show testServerPort
    streamingPostTest 1000 $ "http://localhost:" ++ show testServerPort ++ "/large-post"
    streamingGetTest  hugeLen $ "http://localhost:" ++ show testServerPort
    streamingPostTest hugeLen $ "http://localhost:" ++ show testServerPort ++ "/large-post"
  where
    testProxySettings = Network.HTTP.Proxy.defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }


streamingGetTest :: Int64 -> String -> ResourceT IO ()
streamingGetTest size url = do
    operationSizeMsgR "GET " size
    request <-
            (\r -> r { HC.checkStatus = \ _ _ -> Nothing })
                <$> lift (HC.parseUrl $ url ++ "/large-get?" ++ show size)
    httpCheckGetBodySize $ HC.addProxy "localhost" testProxyPort request
    printPassR


httpCheckGetBodySize :: HC.Request (ResourceT IO) -> ResourceT IO ()
httpCheckGetBodySize req = liftIO $ HC.withManager $ \mgr -> do
    HC.Response st _ hdrs bdy <- HC.http req mgr
    when (st /= HT.status200) $
        error $ "httpCheckGetBodySize : Bad status code : " ++ show st
    let contentLength = readDecimal_ $ fromMaybe "0" $ lookup "content-length" hdrs
    when (contentLength == (0 :: Int64)) $
        error "httpCheckGetBodySize : content-length is zero."
    bdy $$ byteSink contentLength

--------------------------------------------------------------------------------

streamingPostTest :: Int64 -> String -> ResourceT IO ()
streamingPostTest size url = do
    operationSizeMsgR "POST" size
    request <-
            (\r -> r { HC.method = "POST"
                     , HC.requestBody = requestBodySource size
                     -- Disable expecptions for non-2XX status codes.
                     , HC.checkStatus = \ _ _ -> Nothing
                     })
                <$> lift (HC.parseUrl url)
    httpCheckPostResponse size $ HC.addProxy "localhost" testProxyPort request
    printPassR

httpCheckPostResponse :: Int64 -> HC.Request (ResourceT IO) -> ResourceT IO ()
httpCheckPostResponse postLen req = liftIO $ HC.withManager $ \mgr -> do
    HC.Response st _ _ bdy <- HC.http req mgr
    when (st /= HT.status200) $
        error $ "httpCheckGetBodySize : Bad status code : " ++ show st
    bodyText <- bdy $$ CB.take 1024
    let len = case BS.split ':' (BS.concat (LBS.toChunks bodyText)) of
                ["Post-size", size] -> readDecimal_ $ BS.dropWhile isSpace size
                _ -> error "httpCheckPostResponse : Not able to read Post-size."
    when (len /= postLen) $
        error $ "httpCheckPostResponse : Post length " ++ show len ++ " should have been " ++ show postLen ++ "."

--------------------------------------------------------------------------------

requestBodySource :: MonadIO m => Int64 -> HC.RequestBody m
requestBodySource len =
    HC.RequestBodySource len $ DC.sourceState 0 run
  where
    run :: MonadIO m => Int64 -> m (DC.SourceStateResult Int64 Builder)
    run count
        | count >= len = return DC.StateClosed
        | len - count > blockSize64 =
            return $ DC.StateOpen (count + blockSize64) bbytes
        | otherwise =
            let n = len - count
            in return $ DC.StateOpen (count + n) $ fromByteString $ BS.take blockSize bsbytes

    blockSize = 4096
    blockSize64 = fromIntegral blockSize :: Int64
    bsbytes = BS.replicate blockSize '?'
    bbytes = fromByteString bsbytes


--------------------------------------------------------------------------------

warpTlsTest :: IO ()
warpTlsTest = runResourceT $ do
    printTestMsgR "Test Warp with TLS"
    _ <- allocate (forkIO $ runTestServerTLS testServerPort) killThread
    let url = "https://localhost:" ++ show testServerPort ++ "/"
    request <- lift $ HC.parseUrl url
    direct@(Result _ _ hdrs _) <- httpRun request
    let isWarp =
            case lookup "server" hdrs of
                Just s -> BS.isPrefixOf "Warp" s
                Nothing -> False
    unless isWarp $ error "No 'Server: Warp' header."
    when debug $ liftIO $ printResult direct
    printPassR


httpsConnectTest :: IO ()
httpsConnectTest = runResourceT $ do
    printTestMsgR "HTTPS CONNECT test"
    -- Don't need to do anything with these ThreadIds
    _ <- allocate (forkIO $ runTestServerTLS testServerPort) killThread
    _ <- allocate (forkIO $ runProxySettings testProxySettings) killThread
    mapM_ (testSingleUrl debug) tests
    printPassR
  where
    testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
    tests =
        [ ( HT.methodGet,  "https://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show testServerPort ++ "/", Just "Message\n" )
        , ( HT.methodGet,  "https://localhost:" ++ show testServerPort ++ "/forbidden", Nothing )
        ]
