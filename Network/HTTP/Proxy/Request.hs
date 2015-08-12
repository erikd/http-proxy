{-# LANGUAGE CPP, OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Network.HTTP.Proxy.Request
    ( Port
    , Request (..)

    , proxyRequest
    , waiRequest
    , waiRequestHost
    )
    where

import Data.ByteString.Char8 (ByteString)

import Data.Maybe

import           Network.HTTP.Types (Method)
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai

type Port = Int


-- |
data Request = Request
    {
    -- | Request method such as GET.
      requestMethod :: Method
    -- | HTTP version such as 1.1.
    , httpVersion :: HT.HttpVersion
    -- | A list of header (a pair of key and value) in an HTTP request.
    , requestHeaders :: HT.RequestHeaders
    -- | The part of the URL before the query part.
    , requestPath :: ByteString
    -- | Parsed query string information
    , queryString :: HT.Query
    }

proxyRequest :: Wai.Request -> Request
proxyRequest w = Request method'
                         version'
                         headers'
                         path'
                         query'
         where
    method'  = Wai.requestMethod w
    version' = Wai.httpVersion w
    headers' = Wai.requestHeaders w
    path'    = Wai.rawPathInfo w
    query'   = Wai.queryString w

waiRequest :: Request -> Wai.Request
waiRequest r = Wai.defaultRequest
    { Wai.requestMethod  = requestMethod r
    , Wai.httpVersion    = httpVersion r
    , Wai.requestHeaders = requestHeaders r
    , Wai.rawPathInfo    = requestPath r
    , Wai.queryString    = queryString r
    }

waiRequestHost :: Wai.Request -> ByteString
waiRequestHost req = fromMaybe "???" $ Wai.requestHeaderHost req
