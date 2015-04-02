{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.Util where

import Blaze.ByteString.Builder
import Control.Concurrent.Async
import Control.Exception hiding (assert)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (fromString)
import Network.Connection

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit as DC
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai

import Network.HTTP.Proxy.Request

import Test.Request
import Test.ServerDef


dumpWaiRequest :: Wai.Request -> IO ()
dumpWaiRequest req =
    mapM_ BS.putStrLn
            [ "------- Wai Request --------------------------------------------------------------"
            , "Method          : " , Wai.requestMethod req
            , "HTTP Version    : " , fromString (show (Wai.httpVersion req))
            , "Path Info       : " , Wai.rawPathInfo req
            , "Query String    : " , Wai.rawQueryString req
            , "Server          : " , waiRequestHost req
            , "Secure (SSL)    : " , fromString (show (Wai.isSecure req))
            , "Remote Host     : " , fromString (show (Wai.remoteHost req))
            , "Request Headers :"
            , headerShow (Wai.requestHeaders req)
            ]


dumpHttpConduitRequest :: HC.Request -> IO ()
dumpHttpConduitRequest req =
    mapM_ BS.putStrLn
            [ "------- HttpConduit Request ------------------------------------------------------"
            , "Method          : " , HC.method req
            , "Secure (SSL)    : " , fromString (show (HC.secure req))
            , "Host Name       : " , HC.host req
            , "Host Port       : " , fromString (show (HC.port req))
            , "Path            : " , HC.path req
            , "Query String    : " , HT.urlDecode False (HC.queryString req)
            , "Request Headers :"
            , headerShow (HC.requestHeaders req)
            ]


dumpHttpResponse :: HT.Status -> HT.ResponseHeaders -> IO ()
dumpHttpResponse s rh = do
    mapM_ BS.putStrLn
        [ "------- Response from upsteam ----------------------------------------------------"
        , "HTTP/1.0 ", BS.pack (show (HT.statusCode s)), " ", HT.statusMessage s
        ]
    BS.putStr . BS.concat $ map (\ (f, v) -> BS.concat [ "    ", CI.original f, ": ", v, "\n" ]) rh


headerShow :: [HT.Header] -> ByteString
headerShow headers =
    BS.concat $ map hdrShow headers
  where
    hdrShow (f, v) = BS.concat [ "  ", CI.original f , ": " , v, "\n" ]

--------------------------------------------------------------------------------

data Result = Result Int [HT.Header] ByteString


printResult :: Result -> IO ()
printResult (Result status headers body) = do
    putStrLn $ "Response status : " ++ show status
    putStrLn "Response headers :"
    BS.putStr $ headerShow headers
    putStrLn "Response body :"
    BS.putStrLn body

--------------------------------------------------------------------------------

-- | Compare results and error out if they're different.
compareResult :: Result -> Result -> IO ()
compareResult (Result sa ha ba) (Result sb hb bb) = do
    assert (sa == sb) $ "HTTP status codes don't match : " ++ show sa ++ " /= " ++ show sb
    forM_ [ "server", "content-type", "content-length" ] $ \v ->
        assertMaybe (lookup v ha) (lookup v hb) $ \ ja jb ->
            "Header field '" ++ show v ++ "' doesn't match : '" ++ show ja ++ "' /= '" ++ show jb
    assert (ba == bb) $ "HTTP response bodies are different :\n" ++ BS.unpack ba ++ "\n-----------\n" ++ BS.unpack bb
  where
    assert :: Bool -> String -> IO ()
    assert b msg = unless b $ error msg

    assertMaybe :: Eq a => Maybe a -> Maybe a -> (a -> a -> String) -> IO ()
    assertMaybe Nothing _ _ = return ()
    assertMaybe _ Nothing _ = return ()
    assertMaybe (Just a) (Just b) fmsg = unless (a == b) . error $ fmsg a b


testSingleUrl :: Bool -> TestRequest -> IO ()
testSingleUrl debug testreq = do
    request <- setupRequest testreq
    direct <- httpRun request
    proxy <- httpRun $ HC.addProxy "localhost" testProxyPort request
    when debug $ do
        printResult direct
        printResult proxy
    compareResult direct proxy


setupRequest :: TestRequest -> IO HC.Request
setupRequest (method, url, reqBody) = do
    req <- HC.parseUrl url
    return $ req
        { HC.method = if HC.method req /= method then method else HC.method req
        , HC.requestBody = case reqBody of
                            Just x -> HC.RequestBodyBS x
                            Nothing -> HC.requestBody req
        -- In this test program we want to pass error pages back to the test
        -- function so the error output can be compared.
        , HC.checkStatus = \ _ _ _ -> Nothing
        }


-- | Use HC.http to fullfil a HC.Request. We need to wrap it because the
-- Response contains a Source which we need to read to generate our result.
httpRun :: HC.Request -> IO Result
httpRun req = HC.withManagerSettings settings $ \mgr -> do
    resp <- HC.http req mgr
    (_body, finalizer) <- DC.unwrapResumable $ HC.responseBody resp
    bodyText <- HC.responseBody resp DC.$$+- CB.take 819200000
    finalizer
    return $ Result (HT.statusCode $ HC.responseStatus resp)
                (HC.responseHeaders resp) $ BS.concat $ LBS.toChunks bodyText
  where
    settings = HC.mkManagerSettings (TLSSettingsSimple True False False) Nothing



byteSource :: Int64 -> DC.Source (ResourceT IO) (DC.Flush Builder)
byteSource bytes = loop 0
  where
    loop :: MonadIO m => Int64 -> DC.Source m (DC.Flush Builder)
    loop count
        | count >= bytes = return ()
        | bytes - count > blockSize64 = do
            DC.yield $ DC.Chunk bbytes
            loop $ count + blockSize64
        | otherwise = do
            let n = fromIntegral $ bytes - count
            DC.yield . DC.Chunk . fromByteString $ BS.take n bsbytes
            return ()

    blockSize = 8192
    blockSize64 = fromIntegral blockSize :: Int64
    bsbytes = BS.replicate blockSize '?'
    bbytes = fromByteString bsbytes


byteSink :: Int64 -> DC.Sink ByteString (ResourceT IO) ()
byteSink bytes = sink 0
  where
    sink :: Monad m => Int64 -> DC.Sink ByteString m ()
    sink count = do
        mbs <- DC.await
        case mbs of
            Nothing -> close count
            Just bs -> sink (count + fromIntegral (BS.length bs))

    close :: Monad m => Int64 -> m ()
    close count =
        when (count /= bytes) .
            error $ "httpCheckGetBodySize : Body length " ++ show count ++ " should have been " ++ show bytes


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE =
    withAsync action waitCatch >>= either onE return
