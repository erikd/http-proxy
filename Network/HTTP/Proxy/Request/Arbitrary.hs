{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
------------------------------------------------------------
-- Copyright : Ambiata Pty Ltd
-- Author : Sharif Olorin <sio@tesser.org>
-- License : BSD3
------------------------------------------------------------

module Network.HTTP.Proxy.Request.Arbitrary(
    Request ()
) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.Monoid
import Control.Applicative
import Network.HTTP.Proxy.Request
import Network.HTTP.Types
import Test.QuickCheck


stdMethod :: Gen ByteString
stdMethod = elements [ "GET"
                     , "POST"
                     , "HEAD"
                     , "PUT"
                     , "DELETE"
                     , "TRACE"
                     , "CONNECT"
                     , "OPTIONS"
                     , "PATCH"
                     ]

instance Arbitrary HttpVersion where
    arbitrary = elements [ http09
                         , http10
                         , http11
                         ]

ascii :: Gen ByteString
ascii = BS.pack <$> (listOf1 (oneof [choose ('a', 'z'), choose ('0', '9')]))

simpleUri :: Gen ByteString
simpleUri = do
    scheme' <- elements ["http://", "https://"]
    host' <- listOf1 ascii
    port' <- oneof [Just <$> ((arbitrary :: Gen Int) `suchThat` (> 0)), pure Nothing]
    path' <- listOf ascii
    pure . BS.concat $
        [ scheme'
        , BS.intercalate "." host'
        , maybe "" (BS.pack . ((:) ':') . show) port'
        , "/" <> (BS.intercalate "/" path')
        ]

-- The logic here should probably go into the Request type itself at some point.
instance Arbitrary Request where
    arbitrary = do
        method' <- stdMethod
        version' <- arbitrary
        uri' <- simpleUri
        headers' <- listOf header
        qs' <- listOf qi
        pure $ Request method'
                       version'
                       headers'
                       uri'
                       qs'
      where
        header :: Gen Header
        header = (,) <$> ci <*> ascii

        qi :: Gen QueryItem
        qi = (,) <$> ascii <*> oneof [Just <$> ascii, pure Nothing]

        ci :: Gen (CI ByteString)
        ci = mk <$> ascii
