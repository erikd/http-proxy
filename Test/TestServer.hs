{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module Test.TestServer
    ( runTestServer
    , runTestServerTLS
    ) where

import Blaze.ByteString.Builder
import Control.Monad.Trans.Resource
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Data.ByteString (ByteString)
import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Conduit (($$))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit as DC

import Test.Util


runTestServer :: Int -> IO ()
runTestServer port =
    let settings = defaultSettings { settingsPort = port, settingsHost = "*6" }
    in catchAny (runSettings settings serverApp) $ print

runTestServerTLS :: Int -> IO ()
runTestServerTLS port =
    let settings = defaultSettings { settingsPort = port, settingsHost = "*6" }
        tlsSettings' = tlsSettings "Test/certificate.pem" "Test/key.pem"
    in catchAny (runTLS tlsSettings' settings serverApp) $ print

--------------------------------------------------------------------------------

serverApp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serverApp req respond
 | rawPathInfo req == "/forbidden" = do
    let text = "This is the forbidden message.\n"
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString $ show $ BS.length text)
            ]
    respond . responseLBS status403 respHeaders $ LBS.fromChunks [text]

 | rawPathInfo req == "/large-get" = do
    let len = readDecimal_ $ BS.drop 1 $ rawQueryString req
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString $ show len)
            ]
    respond . responseStream status200 respHeaders $ streamingByteSource len

 | rawPathInfo req == "/large-post"
    && requestMethod req == "POST" = do
        let len = readDecimal_ $ fromMaybe "0" $ lookup "content-length" $ requestHeaders req :: Int64
        error $ if len == 0 then "serverApp #3" else "serverApp #4"

 | otherwise = do
    let text = BS.concat
            [ "  Method          : " , requestMethod req , "\n"
            , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
            , "  Path Info       : " , rawPathInfo req , "\n"
            , "  Query String    : " , rawQueryString req , "\n"
          --, "  Server Name     : " , serverName req , "\n"
          --, "  Server Port     : " , fromString (show (serverPort req)), "\n"
            , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
            , "  Request Headers :\n\n"
            , headerShow (requestHeaders req)
            , "\n"
            ]
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString $ show $ BS.length text)
            ]
    respond . responseLBS status200 respHeaders $ LBS.fromChunks [text]

streamingByteSource :: Int -> (Builder -> IO ()) -> IO () -> IO ()
streamingByteSource len write flush =
    let loop remaining
            | remaining > 0 = do
                let current = min 2048 remaining
                write . fromByteString $ BS.replicate current 'a'
                flush
                loop (remaining - current)
            | otherwise = return ()
    in loop len

-- Network.Wai provides a responseLBS (build a response from a lazy ByteString).
-- In this case we had a strict ByteString so I modified responseLBS to
-- produce this version.
responseBS :: Status -> ResponseHeaders -> ByteString -> Response
responseBS s h = error "responsBS" -- ResponseBuilder s h . fromByteString


largePostLenZero :: ResourceT IO Response
largePostLenZero = do
    let text = "Error : POST Content-Length was either missing or zero.\n"
    let respHeaders =
                [ (hContentType, "text/plain")
                , (hContentLength, fromString $ show $ BS.length text)
                ]
    return $ responseBS status400 respHeaders text


largePostCheck :: Int64 -> DC.Source (ResourceT IO) ByteString -> ResourceT IO Response
largePostCheck len rbody = do
    rbody $$ byteSink len
    let text = BS.pack $ "Post-size:" ++ show len
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString $ show $ BS.length text)
            ]
    return $ responseBS status200 respHeaders text

