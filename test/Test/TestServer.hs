{-# LANGUAGE CPP, OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.TestServer
    ( runTestServer
    , runTestServerTLS
    ) where

import Data.ByteString (ByteString)
import Data.Conduit (ConduitT)
import Data.List (sort)
#if ! MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Conduit ((.|))
import Data.Int (Int64)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit as DC

import qualified Network.HTTP.Proxy.Request as HPR

import Test.ServerDef
import Test.Util


runTestServer :: IO ()
runTestServer =
    let settings = setPort (httpTestPort portsDef) $ setHost "*6" defaultSettings
    in catchAny (runSettings settings serverApp) print

runTestServerTLS :: IO ()
runTestServerTLS =
    let settings = setPort (httpsTestPort portsDef) $ setHost "*6" defaultSettings
        tlsSettings' = tlsSettings "test/certificate.pem" "test/key.pem"
    in catchAny (runTLS tlsSettings' settings serverApp) print

--------------------------------------------------------------------------------

serverApp :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serverApp req respond
    | rawPathInfo req == "/forbidden" =
        respond $ simpleResponse status403 "This is the forbidden message.\n"

    | rawPathInfo req == "/301" = do
        let respHeaders = [ (hLocation, "http://other-server" <> rawPathInfo req) ]
        respond $ responseLBS status301 respHeaders mempty

    | rawPathInfo req == "/large-get" = do
        let len = readDecimal_ $ BS.drop 1 $ rawQueryString req
        let respHeaders =
                [ (hContentType, "text/plain")
                , (hContentLength, fromString $ show len)
                ]
        respond . responseSource status200 respHeaders $ builderSource len

    | rawPathInfo req == "/secure" = do
        let body = "Using SSL: " <> BS.pack (show $ isSecure req)
        let respHeaders =
                [ (hContentType, "text/plain")
                , (hContentLength, fromString . show $ BS.length body)
                ]
        respond $ responseBS status200 respHeaders body

    | rawPathInfo req == "/large-post" && requestMethod req == "POST" = do
        let len = maybe 0 readDecimal_ (lookup "content-length" $ requestHeaders req) :: Int64
        if len == 0
            then respond $ simpleResponse status400 "Error : POST Content-Length was either missing or zero.\n"
            else respond =<< largePostCheck len (sourceRequestBody req)

    | otherwise = do
        let text = "This is the not-found message.\n\n" : responseBody req
            respHeaders = [ (hContentType, "text/plain") ]
        respond . responseLBS status404 respHeaders $ LBS.fromChunks text


responseBody :: Request -> [ByteString]
responseBody req =
    [ "  Method          : " , requestMethod req , "\n"
    , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
    , "  Path Info       : " , rawPathInfo req , "\n"
    , "  Query String    : " , rawQueryString req , "\n"
    , "  Server          : " , HPR.waiRequestHost req , "\n"
    , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
    , "  Request Headers :\n"
    , headerShow (sort $ requestHeaders req)
    , "\n"
    ]


largePostCheck :: Int64 -> ConduitT () ByteString IO () -> IO Response
largePostCheck len rbody =
    maybe success failure <$> (DC.runConduit $ rbody .| byteSink len)
  where
    success = simpleResponse status200 . BS.pack $ "Post-size: " ++ show len
    failure = simpleResponse status500
