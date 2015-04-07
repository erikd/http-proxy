{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.Request
    ( UriScheme (..)
    , mkGetRequest
    , mkGetRequestWithBody
    , mkPostRequest
    , mkPostRequestWithBody
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char
import Data.Maybe

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Test.ServerDef


data UriScheme
    = Http | Https
    deriving Show


mkGetRequest :: UriScheme -> String -> IO HC.Request
mkGetRequest scheme path = mkTestRequest get scheme path Nothing


mkGetRequestWithBody :: UriScheme -> String -> ByteString -> IO HC.Request
mkGetRequestWithBody scheme path body = mkTestRequestBS get scheme path (Just body)


mkPostRequest :: UriScheme -> String -> IO HC.Request
mkPostRequest scheme path = mkTestRequest post scheme path Nothing


mkPostRequestWithBody :: UriScheme -> String -> ByteString -> IO HC.Request
mkPostRequestWithBody scheme path body = mkTestRequestBS post scheme path (Just body)


mkTestRequestBS :: HT.Method -> UriScheme -> String -> Maybe ByteString -> IO HC.Request
mkTestRequestBS method scheme path mbody = mkTestRequest method scheme path $ HC.RequestBodyBS <$> mbody


mkTestRequest :: HT.Method -> UriScheme -> String -> Maybe HC.RequestBody -> IO HC.Request
mkTestRequest method scheme path mbody = do
    let port = show $ case scheme of
                        Http -> httpTestPort
                        Https -> httpsTestPort
        url = map toLower (show scheme) ++ "://localhost:" ++ port ++ path
    req <- HC.parseUrl url
    return $ req
        { HC.method = if HC.method req /= method then method else HC.method req
        , HC.requestBody = fromMaybe (HC.requestBody req) mbody
        -- In this test program we want to pass error pages back to the test
        -- function so the error output can be compared.
        , HC.checkStatus = \ _ _ _ -> Nothing
        }


get, post :: HT.Method
get = HT.methodGet
post = HT.methodPost

