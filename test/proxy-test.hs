{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

import Blaze.ByteString.Builder
import Network.HTTP.Proxy

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (unless, when)
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

import Test.TestServer
import Test.HttpHttpsRewriteTest
import Test.Util


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
basicTest = do
    printTestMsgR "Basic tests"
    withTestServerAndProxy httpTestPort testProxySettings $ do
        mapM_ (testSingleUrl debug) tests
        printPassR
  where
    testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
    tests =
        [ ( HT.methodGet,  "http://localhost:" ++ show httpTestPort ++ "/", Nothing )
        , ( HT.methodPost, "http://localhost:" ++ show httpTestPort ++ "/", Nothing )
        , ( HT.methodPost, "http://localhost:" ++ show httpTestPort ++ "/", Just "Message\n" )
        , ( HT.methodGet,  "http://localhost:" ++ show httpTestPort ++ "/forbidden", Nothing )
        ]

--------------------------------------------------------------------------------

streamingTest :: IO ()
streamingTest =
    withTestServerAndProxy httpTestPort testProxySettings $ do
        streamingGetTest  1000 $ "http://localhost:" ++ show httpTestPort
        streamingPostTest 1000 $ "http://localhost:" ++ show httpTestPort ++ "/large-post"
        streamingGetTest  hugeLen $ "http://localhost:" ++ show httpTestPort
        streamingPostTest hugeLen $ "http://localhost:" ++ show httpTestPort ++ "/large-post"
        printPassR
  where
    testProxySettings = Network.HTTP.Proxy.defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }


streamingGetTest :: Int64 -> String -> IO ()
streamingGetTest size url = do
    operationSizeMsgR "GET " size
    request <-
            (\r -> r { HC.checkStatus = \ _ _ _ -> Nothing })
                <$> HC.parseUrl (url ++ "/large-get?" ++ show size)
    httpCheckGetBodySize $ HC.addProxy "localhost" testProxyPort request
    printPassR


httpCheckGetBodySize :: HC.Request -> IO ()
httpCheckGetBodySize req = HC.withManager $ \mgr -> do
    resp <- HC.http req mgr
    let (st, hdrs, bdyR) = (HC.responseStatus resp, HC.responseHeaders resp, HC.responseBody resp)
    when (st /= HT.status200) $
        error $ "httpCheckGetBodySize : Bad status code : " ++ show st
    let contentLength = readDecimal_ $ fromMaybe "0" $ lookup "content-length" hdrs
    when (contentLength == (0 :: Int64)) $
        error "httpCheckGetBodySize : content-length is zero."
    (bdy, finalizer) <- DC.unwrapResumable bdyR
    bdy $$ byteSink contentLength
    finalizer

--------------------------------------------------------------------------------

streamingPostTest :: Int64 -> String -> IO ()
streamingPostTest size url = do
    operationSizeMsgR "POST" size
    request <-
            (\r -> r { HC.method = "POST"
                     , HC.requestBody = requestBodySource size
                     -- Disable expecptions for non-2XX status codes.
                     , HC.checkStatus = \ _ _ _ -> Nothing
                     })
                <$> HC.parseUrl url
    httpCheckPostResponse size $ HC.addProxy "localhost" testProxyPort request
    printPassR

httpCheckPostResponse :: Int64 -> HC.Request -> IO ()
httpCheckPostResponse postLen req = HC.withManager $ \mgr -> do
    resp <- HC.http req mgr
    let (st, bodyR) = (HC.responseStatus resp, HC.responseBody resp)
    when (st /= HT.status200) $
        error $ "httpCheckGetBodySize : Bad status code : " ++ show st
    (bdy, finalizer) <- DC.unwrapResumable bodyR
    bodyText <- bdy $$ CB.take 1024
    finalizer
    let len = case BS.split ':' (BS.concat (LBS.toChunks bodyText)) of
                ["Post-size", size] -> readDecimal_ $ BS.dropWhile isSpace size
                _ -> error "httpCheckPostResponse : Not able to read Post-size."
    when (len /= postLen) $
        error $ "httpCheckPostResponse : Post length " ++ show len ++ " should have been " ++ show postLen ++ "."

--------------------------------------------------------------------------------

requestBodySource :: Int64 -> HC.RequestBody
requestBodySource len =
    error "requestBoddySource" len $ loop 0
    -- HC.RequestBodyStream len $ loop 0
  where
    loop :: Int64 -> DC.Source IO Builder
    loop count
        | count >= len = return ()
        | len - count > blockSize64 = do
            DC.yield bbytes
            loop $ count + blockSize64
        | otherwise = do
            let n = fromIntegral $ len - count
            DC.yield $ fromByteString $ BS.take n bsbytes
            return ()

    blockSize = 4096
    blockSize64 = fromIntegral blockSize :: Int64
    bsbytes = BS.replicate blockSize '?'
    bbytes = fromByteString bsbytes


--------------------------------------------------------------------------------

warpTlsTest :: IO ()
warpTlsTest = do
    printTestMsgR "Test Warp with TLS"
    th <- forkIO $ runTestServerTLS httpTestPort
    let url = "https://localhost:" ++ show httpTestPort ++ "/"
    request <- HC.parseUrl url
    direct@(Result _ _ hdrs _) <- httpRun request
    let isWarp =
            case lookup "server" hdrs of
                Just s -> BS.isPrefixOf "Warp" s
                Nothing -> False
    unless isWarp $ error "No 'Server: Warp' header."
    when debug $ printResult direct
    killThread th
    printPassR


httpsConnectTest :: IO ()
httpsConnectTest = do
    printTestMsgR "HTTPS CONNECT test"
    withTestServerAndProxy httpsTestPort testProxySettings $
        mapM_ (testSingleUrl debug) tests
    printPassR
  where
    testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
    tests =
        [ ( HT.methodGet,  "https://localhost:" ++ show httpTestPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show httpTestPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show httpTestPort ++ "/", Just "Message\n" )
        , ( HT.methodGet,  "https://localhost:" ++ show httpTestPort ++ "/forbidden", Nothing )
        ]

withTestServerAndProxy :: Int -> Settings -> IO a -> IO a
withTestServerAndProxy httpTestPort testProxySettings action =
    withAsync (runTestServer httpTestPort) $ \ async1 ->
        withAsync (runTestProxy testProxySettings) $ \ async2 -> do
            res <- action
            cancel async1
            cancel async2
            return res


runTestProxy :: Settings -> IO ()
runTestProxy settings =
    catchAny (runProxySettings settings) $ \ _ -> return ()
  where
    tryAny :: IO a -> IO (Either SomeException a)
    tryAny action = withAsync action waitCatch

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny action onE = tryAny action >>= either onE return
