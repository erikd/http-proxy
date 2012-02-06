{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module TestServer
    ( runTestServer
    , byteSink
    ) where

import Blaze.ByteString.Builder
import Control.Monad.Trans.Resource
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lex.Integral (readDecimal_)
import Data.Conduit (($$))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit as DC

import Util


runTestServer :: Int -> IO ()
runTestServer port =
    let settings = defaultSettings { settingsPort = port }
    in runSettings settings serverApp


serverApp :: Request -> ResourceT IO Response
serverApp req
 | rawPathInfo req == "/forbidden" = do
    let text = "This is the forbidden message.\n"
    let respHeaders =
            [ headerContentType "text/plain"
            , headerContentLength $ fromString $ show $ BS.length text
            ]
    return $ responseBS statusForbidden respHeaders text

 | rawPathInfo req == "/large-get" = do
    let len = readDecimal_ $ BS.drop 1 $ rawQueryString req
    let respHeaders =
            [ headerContentType "text/plain"
            , headerContentLength $ fromString $ show len
            ]
    return $ ResponseSource statusOK respHeaders $ byteSource len

 | rawPathInfo req == "/large-post"
    && requestMethod req == "POST" = do
        let len = readDecimal_ $ fromMaybe "0" $ lookup "content-length" $ requestHeaders req :: Int64
        if len == 0
            then largePostLenZero
            else largePostCheck len $ requestBody req


 | otherwise = do
    let text = BS.concat
            [ "  Method          : " , requestMethod req , "\n"
            , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
            , "  Path Info       : " , rawPathInfo req , "\n"
            , "  Query String    : " , rawQueryString req , "\n"
            , "  Server Name     : " , serverName req , "\n"
            , "  Server Port     : " , fromString (show (serverPort req)), "\n"
            , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
            , "  Request Headers :\n\n"
            , headerShow (requestHeaders req)
            , "\n\n"
            ]
    let respHeaders =
            [ headerContentType "text/plain"
            , headerContentLength $ fromString $ show $ BS.length text
            ]
    return $ responseBS statusOK respHeaders text


-- Network.Wai provides a responseLBS (build a response from a lazy ByteString).
-- In this case we had a strict ByteString so I modified responseLBS to
-- produce this version.
responseBS :: Status -> ResponseHeaders -> ByteString -> Response
responseBS s h = ResponseBuilder s h . fromByteString


largePostLenZero :: ResourceT IO Response
largePostLenZero = do
    let text = "Error : POST Content-Length was either missing or zero.\n"
    let respHeaders =
                [ headerContentType "text/plain"
                , headerContentLength $ fromString $ show $ BS.length text
                ]
    return $ responseBS statusBadRequest respHeaders text


largePostCheck :: Int64 -> DC.Source IO ByteString -> ResourceT IO Response
largePostCheck len rbody = do
    rbody $$ byteSink len
    let text = BS.pack $ "Post-size:" ++ show len
    let respHeaders =
            [ headerContentType "text/plain"
            , headerContentLength $ fromString $ show $ BS.length text
            ]
    return $ responseBS statusOK respHeaders text

--------------------------------------------------------------------------------

byteSource :: (MonadIO m, Resource m) => Int64 -> DC.Source m (DC.Flush Builder)
byteSource bytes = DC.sourceState 0 go
  where
    go :: MonadIO m => Int64 -> ResourceT m (DC.SourceStateResult Int64 (DC.Flush Builder))
    go count
        | count >= bytes = return DC.StateClosed
        | bytes - count > blockSize64 =
            return $ DC.StateOpen (count + blockSize64) $ DC.Chunk bbytes
        | otherwise =
            let n = bytes - count
            in return   $ DC.StateOpen (count + n)
                        $ DC.Chunk $ fromByteString $ BS.take blockSize bsbytes

    blockSize = 8192
    blockSize64 = fromIntegral blockSize :: Int64
    bsbytes = BS.replicate blockSize '?'
    bbytes = fromByteString bsbytes


byteSink :: Resource m => Int64 -> DC.Sink ByteString m ()
byteSink bytes =
    DC.sinkState 0 sink close
  where
    sink :: Resource m => Int64 -> ByteString -> ResourceT m (DC.SinkStateResult Int64 ByteString ())
    sink count bs =
        if BS.null bs
            then return $ DC.StateDone Nothing ()
            else return $ DC.StateProcessing (count + fromIntegral (BS.length bs))

    close :: Resource m => Int64 -> ResourceT m ()
    close count =
        when (count /= bytes) $
            error $ "httpCheckGetBodySize : Body length " ++ show count ++ " should have been " ++ show bytes
