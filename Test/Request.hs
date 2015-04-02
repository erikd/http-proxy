{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.Request
    ( UriScheme (..)
    , TestRequest
    , mkGetRequest
    , mkGetRequestWithBody
    , mkPostRequest
    , mkPostRequestWithBody
    ) where

import Data.ByteString (ByteString)
import Data.Char

import qualified Network.HTTP.Types as HT

import Test.ServerDef


data UriScheme
    = Http | Https
    deriving Show

type TestRequest = ( HT.Method, String, Maybe ByteString )


mkGetRequest :: UriScheme -> String -> TestRequest
mkGetRequest scheme path = mkTestRequest get scheme path Nothing

mkGetRequestWithBody :: UriScheme -> String -> ByteString -> TestRequest
mkGetRequestWithBody scheme path body = mkTestRequest get scheme path (Just body)

mkPostRequest :: UriScheme -> String -> TestRequest
mkPostRequest scheme path = mkTestRequest post scheme path Nothing

mkPostRequestWithBody :: UriScheme -> String -> ByteString -> TestRequest
mkPostRequestWithBody scheme path body = mkTestRequest post scheme path (Just body)

mkTestRequest :: HT.Method -> UriScheme -> String -> Maybe ByteString -> TestRequest
mkTestRequest meth scheme path body =
    let port = show $ case scheme of
                        Http -> httpTestPort
                        Https -> httpsTestPort
    in  ( meth
        , map toLower (show scheme) ++ "://localhost:" ++ port ++ path
        , body
        )

get, post :: HT.Method
get = HT.methodGet
post = HT.methodPost

