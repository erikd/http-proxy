{-# LANGUAGE BangPatterns, OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.Util where

import Blaze.ByteString.Builder
import Control.Concurrent.Async
import Control.Exception hiding (assert)
import Control.Monad (forM_, when, unless)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, Flush (..), SealedConduitT)
import Data.Int (Int64)
import Data.Maybe
import Data.String (fromString)
import Network.Socket
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
    hdrShow (f, v) = BS.concat [ "    ", CI.original f , ": " , v, "\n" ]


--------------------------------------------------------------------------------

simpleResponse :: HT.Status -> ByteString -> Wai.Response
simpleResponse status text = do
    let respHeaders =
            [ (HT.hContentType, "text/plain")
            , (HT.hContentLength, fromString . show $ BS.length text)
            ]
    responseBS status respHeaders text


responseBS :: HT.Status -> HT.ResponseHeaders -> ByteString -> Wai.Response
responseBS status headers text =
    Wai.responseLBS status headers$ LBS.fromChunks [text]

--------------------------------------------------------------------------------

data Result = Result
    { resultSecure :: Bool
    , resultStatus :: Int
    , resultHeaders :: [HT.Header]
    , resultBS :: ByteString
    }


printResult :: Result -> IO ()
printResult (Result _ status headers body) = do
    putStrLn $ "Response status : " ++ show status
    putStrLn "Response headers :"
    BS.putStr $ headerShow headers
    putStrLn "Response body :"
    BS.putStrLn body

--------------------------------------------------------------------------------

-- | Compare results and error out if they're different.
compareResult :: Result -> Result -> IO ()
compareResult (Result secure sa ha ba) (Result _ sb hb bb) = do
    assert (sa == sb) $ "HTTP status codes don't match : " ++ show sa ++ " /= " ++ show sb
    forM_ [ "server", "content-type", "content-length" ] $ \v ->
        assertMaybe (lookup v ha) (lookup v hb) $ \ ja jb ->
            "Header field '" ++ show v ++ "' doesn't match : '" ++ show ja ++ "' /= '" ++ show jb
    assert (ba == bb) $ "HTTP response bodies are different :\n" ++ BS.unpack ba ++ "\n-----------\n" ++ BS.unpack bb
    when (not secure && isJust (lookup "X-Via-Proxy" ha)) $
        error "Error: Direct connection should not contain 'X-Via-Proxy' header."
    when (not secure && isNothing (lookup "X-Via-Proxy" hb)) $
        error "Error: Direct connection should not contain 'X-Via-Proxy' header."
  where
    assert :: Bool -> String -> IO ()
    assert b = unless b . error

    assertMaybe :: Eq a => Maybe a -> Maybe a -> (a -> a -> String) -> IO ()
    assertMaybe Nothing _ _ = return ()
    assertMaybe _ Nothing _ = return ()
    assertMaybe (Just a) (Just b) fmsg = unless (a == b) . error $ fmsg a b


testSingleUrl :: Bool -> Int -> HC.Request -> IO ()
testSingleUrl debug testProxyPort request = do
    direct <- httpRun request
    proxy <- httpRun $ addTestProxy testProxyPort request
    when debug $ do
        printResult direct
        printResult proxy
    compareResult direct proxy


addTestProxy :: Int -> HC.Request -> HC.Request
addTestProxy = HC.addProxy "localhost"


-- | Use HC.http to fullfil a HC.Request. We need to wrap it because the
-- Response contains a Source which we need to read to generate our result.
httpRun :: HC.Request -> IO Result
httpRun req = do
    mgr <- HC.newManager $ HC.mkManagerSettings (TLSSettingsSimple True False False) Nothing
    runResourceT $ do
        resp <- HC.http (modifyRequest req) mgr
        let contentLen = readInt64 <$> lookup HT.hContentLength (HC.responseHeaders resp)
        bodyText <- checkBodySize (DC.sealConduitT $ HC.responseBody resp) contentLen
        return $ Result (HC.secure req) (HT.statusCode $ HC.responseStatus resp)
                        (HC.responseHeaders resp) bodyText
  where
    modifyRequest r = r { HC.redirectCount = 0  }


checkBodySize :: Monad f => SealedConduitT () ByteString f () -> Maybe Int64 -> f ByteString
checkBodySize bodySrc Nothing = fmap (BS.concat . LBS.toChunks) $ bodySrc DC.$$+- CB.take 1000
checkBodySize bodySrc (Just len) = do
    let blockSize = 1000
    if len <= blockSize
        then checkBodySize bodySrc Nothing
        else fromMaybe "Success" <$> (bodySrc DC.$$+- byteSink len)


byteSink :: Monad m => Int64 -> ConduitT ByteString a m (Maybe ByteString)
byteSink bytes = sink 0
  where
    sink :: Monad m => Int64 -> ConduitT ByteString a m (Maybe ByteString)
    sink !count = DC.await >>= maybe (closeSink count) (sinkBlock count)

    sinkBlock :: Monad m => Int64 -> ByteString -> ConduitT ByteString a m (Maybe ByteString)
    sinkBlock !count bs = sink (count + fromIntegral (BS.length bs))

    closeSink :: Monad m => Int64 -> m (Maybe ByteString)
    closeSink !count = return $
            if count == bytes
                then Nothing
                else Just . BS.pack $ "Error : Body length " ++ show count
                                    ++ " should have been " ++ show bytes ++ "."


builderSource :: Monad m => Int64 -> ConduitT () (Flush Builder) m ()
builderSource = DC.mapOutput (Chunk . fromByteString) . byteSource


byteSource :: Monad m => Int64 -> ConduitT i ByteString m ()
byteSource bytes = loop 0
  where
    loop :: Monad m => Int64 -> ConduitT i ByteString m ()
    loop !count
        | count >= bytes = return ()
        | count + blockSize64 < bytes = do
            DC.yield bsbytes
            loop $ count + blockSize64
        | otherwise = do
            let n = fromIntegral $ bytes - count
            DC.yield $ BS.take n bsbytes
            pure ()

    blockSize = 8192 :: Int
    blockSize64 = fromIntegral blockSize :: Int64
    bsbytes = BS.replicate blockSize '?'


readInt64 :: ByteString -> Int64
readInt64 = read . BS.unpack


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE =
    withAsync action waitCatch >>= either onE return


openLocalhostListenSocket :: IO (Socket, Port)
openLocalhostListenSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") Nothing
    bind sock (addrAddress addr)
    listen sock 10
    port <- fromIntegral <$> socketPort sock
    return (sock, port)

