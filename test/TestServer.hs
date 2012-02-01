{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module TestServer (runTestServer) where

import Blaze.ByteString.Builder.ByteString
import Control.Monad.Trans.Resource
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Data.ByteString                    (ByteString)

import qualified Data.ByteString          as BS


import Util (headerShow)


runTestServer :: Int -> IO ()
runTestServer port =
    let settings = defaultSettings { settingsPort = port }
    in runSettings settings serverApp


serverApp :: Request -> ResourceT IO Response
serverApp req = do
    let text = BS.concat
            [ "  Method          : " , requestMethod req , "\n"
            , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
            , "  Path Info       : " , rawPathInfo req , "\n"
            , "  Query String    : " , rawQueryString req , "\n"
            , "  Server Name     : " , serverName req , "\n"
            , "  Server Port     : " , fromString (show (serverPort req)), "\n"
            , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
            , "  Remote Host     : " , fromString (show (remoteHost req)), "\n"
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


