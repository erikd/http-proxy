{-# LANGUAGE CPP, OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright     : Erik de Castro Lopo
-- Maintainer    : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3
---------------------------------------------------------

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

import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai

type Port = Int


-- |
data Request = Request
    {
    -- | Request method such as GET.
      requestMethod :: HT.Method
    -- | HTTP version such as 1.1.
    , httpVersion :: HT.HttpVersion
    -- | The upstream host name.
    , requestHost :: ByteString
    -- | The port number of the upstream host.
    , requestPort :: Port
    -- | A list of header (a pair of key and value) in an HTTP request.
    , requestHeaders :: HT.RequestHeaders
    -- | The part of the URL after the hostname (and optional port number) and
    -- before the query part.
    , requestPath :: ByteString
    -- | Parsed query string information
    , queryString :: HT.Query
    }


proxyRequest :: Wai.Request -> Request
proxyRequest = error "proxyRequest"

waiRequest :: Request -> Wai.Request
waiRequest = error "waiRequest"

waiRequestHost :: Wai.Request -> ByteString
waiRequestHost req = fromMaybe "???" $ lookup (CI.mk "Host") (Wai.requestHeaders req)
