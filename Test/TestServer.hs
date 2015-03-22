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

import Blaze.ByteString.Builder hiding (flush)
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

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit as DC

import qualified Network.HTTP.Proxy.Request as HPR

import Test.Util


runTestServer :: Int -> IO ()
runTestServer port =
    let settings = setPort port $ setHost "*6" defaultSettings
    in catchAny (runSettings settings serverApp) print

runTestServerTLS :: Int -> IO ()
runTestServerTLS port =
    let settings = setPort port $ setHost "*6" defaultSettings
        tlsSettings' = tlsSettings "Test/certificate.pem" "Test/key.pem"
    in catchAny (runTLS tlsSettings' settings serverApp) print

--------------------------------------------------------------------------------

serverApp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serverApp req respond
    | rawPathInfo req == "/forbidden" =
        respond $ simpleResponse status403 "This is the forbidden message.\n"

    | rawPathInfo req == "/large-get" = do
        let len = readDecimal_ $ BS.drop 1 $ rawQueryString req
        let respHeaders =
                [ (hContentType, "text/plain")
                , (hContentLength, fromString $ show len)
                ]
        respond . responseStream status200 respHeaders $ streamingByteSource len

    | rawPathInfo req == "/large-post" && requestMethod req == "POST" = do
        let len = maybe 0 readDecimal_ (lookup "content-length" $ requestHeaders req) :: Int64
        if len == 0
            then respond $ simpleResponse status400 "Error : POST Content-Length was either missing or zero.\n"
            else error "serverApp #4"

    | otherwise = do
        let text =
                [ "This is the not-found message.\n\n"
                , "  Method          : " , requestMethod req , "\n"
                , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
                , "  Path Info       : " , rawPathInfo req , "\n"
                , "  Query String    : " , rawQueryString req , "\n"
                , "  Server          : " , HPR.waiRequestHost req , "\n"
                , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
                , "  Request Headers :\n"
                , headerShow (requestHeaders req)
                , "\n"
                ]
            respHeaders = [ (hContentType, "text/plain") ]
        respond . responseLBS status404 respHeaders $ LBS.fromChunks text


streamingByteSource :: Int -> (Builder -> IO ()) -> IO () -> IO ()
streamingByteSource len write flush =
    loop len
  where
    loop remaining
        | remaining > 0 = do
            let current = min 2048 remaining
            write . fromByteString $ BS.replicate current 'a'
            flush
            loop $ remaining - current
        | otherwise = return ()


simpleResponse :: Status -> ByteString -> Response
simpleResponse status text = do
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString . show $ BS.length text)
            ]
    responseBS status respHeaders text


responseBS :: Status -> ResponseHeaders -> ByteString -> Response
responseBS status headers text = responseLBS status headers$ LBS.fromChunks [text]


largePostCheck :: Int64 -> DC.Source (ResourceT IO) ByteString -> ResourceT IO Response
largePostCheck len rbody = do
    rbody $$ byteSink len
    let text = BS.pack $ "Post-size:" ++ show len
    let respHeaders =
            [ (hContentType, "text/plain")
            , (hContentLength, fromString $ show $ BS.length text)
            ]
    return $ responseBS status200 respHeaders text

