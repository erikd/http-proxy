{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module Util where

import Control.Monad.Trans.Resource

import Data.ByteString (ByteString)
import Data.String (fromString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai


dumpWaiRequest :: Wai.Request -> IO ()
dumpWaiRequest req =
    let text = BS.concat
            [ "------- Wai Request --------------------------------------------------------------\n"
            , "Method          : " , Wai.requestMethod req , "\n"
            , "HTTP Version    : " , fromString (show (Wai.httpVersion req)) , "\n"
            , "Path Info       : " , Wai.rawPathInfo req , "\n"
            , "Query String    : " , Wai.rawQueryString req , "\n"
            , "Server Name     : " , Wai.serverName req , "\n"
            , "Server Port     : " , fromString (show (Wai.serverPort req)), "\n"
            , "Secure (SSL)    : " , fromString (show (Wai.isSecure req)), "\n"
            , "Remote Host     : " , fromString (show (Wai.remoteHost req)), "\n"
            , "Request Headers :\n"
            , headerShow (Wai.requestHeaders req), "\n"
            ]
    in BS.putStr text


dumpHttpConduitRequest :: HC.Request m -> IO ()
dumpHttpConduitRequest req =
    let text = BS.concat
            [ "------- HttpConduit Request ------------------------------------------------------\n"
            , "Method          : " , HC.method req , "\n"
            , "Secure (SSL)    : " , fromString (show (HC.secure req)), "\n"
            , "Host Name       : " , HC.host req , "\n"
            , "Host Port       : " , fromString (show (HC.port req)), "\n"
            , "Path            : " , HC.path req , "\n"
            , "Query String    : " , HT.urlDecode False (HC.queryString req), "\n"
            , "Request Headers :\n"
            , headerShow (HC.requestHeaders req), "\n"
            ]
    in BS.putStr text


dumpHttpResponse :: HT.Status -> HT.ResponseHeaders -> IO ()
dumpHttpResponse s rh = do
    let text = BS.concat
            [ "------- Response from upsteam ----------------------------------------------------\n"
            , "HTTP/1.0 ", BS.pack (show (HT.statusCode s)), " ", HT.statusMessage s, "\n"
            ]

    BS.putStr text
    BS.putStr $ BS.concat $ map (\ (f, v) -> BS.concat [ "    ", CI.original f, ": ", v, "\n" ]) rh


headerShow :: [HT.Header] -> ByteString
headerShow headers =
    BS.concat $ map hShow headers
  where
    hShow (f, v) = BS.concat [ "  ", CI.original f , ": " , v, "\n" ]

--------------------------------------------------------------------------------

withManagerSettings :: ResourceIO m => HC.ManagerSettings -> (HC.Manager -> ResourceT m a) -> m a
withManagerSettings settings f = runResourceT $ do
    (_, manager) <- withIO (HC.newManager settings) HC.closeManager
    f manager
