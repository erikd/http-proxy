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
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (Method)

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
    , queryString :: ByteString
    } deriving (Show, Eq)


proxyRequest :: Wai.Request -> Request
proxyRequest wreq = Request
                        (Wai.requestMethod wreq)
                        (Wai.httpVersion wreq)
                        (Wai.requestHeaders wreq)
                        (Wai.rawPathInfo wreq)
                        (Wai.rawQueryString wreq)

waiRequest :: Wai.Request -> Request -> Wai.Request
waiRequest original req = original
    { Wai.requestMethod  = requestMethod req
    , Wai.httpVersion    = httpVersion req
    , Wai.requestHeaders = requestHeaders req
    , Wai.rawPathInfo    = requestPath req
    , Wai.rawQueryString = queryString req
    }


waiRequestHost :: Wai.Request -> ByteString
waiRequestHost req = fromMaybe "???" $ Wai.requestHeaderHost req
