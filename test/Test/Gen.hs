{-# LANGUAGE OverloadedStrings    #-}
------------------------------------------------------------
-- Copyright : Ambiata Pty Ltd
-- Author : Sharif Olorin <sio@tesser.org>
-- License : BSD3
------------------------------------------------------------

module Test.Gen
    ( genWaiRequest
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive (CI)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Network.HTTP.Types
import Network.Socket (SockAddr (..), PortNumber)
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Internal as Wai

genWaiRequest :: Gen Wai.Request
genWaiRequest = do
    method <- genHttpMethod
    version <- genHttpVersion
    pathList <- listOf genAscii
    secure <- elements [ False, True ]
    query <- genQuery
    port <- genPort
    sockAddr <- SockAddrInet port <$> arbitrary
    host <- genHostname
    headers <- genHeaderList
    (bodylen, body) <- genRequestBody
    return $ Wai.Request method version
                (BS.concat $ "/" : intersperse "/" pathList)
                (renderQueryBS query)
                headers secure sockAddr
                (map T.decodeUtf8 pathList)
                query
                (return body)   -- requestBody
                Vault.empty
                bodylen         -- requestBodyLength
                (Just host)     -- requestHeaderHost
                Nothing         -- requestHeaderRange
                Nothing         -- requestHeaderReferer
                Nothing         -- requestHeaderUserAgent


genRequestBody :: Gen (Wai.RequestBodyLength, ByteString)
genRequestBody =
    let mkResult body = (Wai.KnownLength (fromIntegral $ BS.length body), body)
    in  mkResult <$> genAscii


genHttpMethod :: Gen ByteString
genHttpMethod = elements
                [ "GET", "POST", "HEAD", "PUT", "DELETE", "TRACE", "CONNECT"
                , "OPTIONS", "PATCH"
                ]

genHttpVersion :: Gen HttpVersion
genHttpVersion = elements [ http09, http10, http11 ]

genHostname :: Gen ByteString
genHostname = BS.intercalate "." <$> listOf1 genAscii

genPort :: Gen PortNumber
genPort = fromIntegral <$> arbitrary `suchThat` (\x -> x > 1024 && x < (65536 :: Int))

genHeaderList :: Gen [Header]
genHeaderList = listOf genHeader

genHeader :: Gen Header
genHeader = (,) <$> genHeaderName <*> genAscii

genHeaderName :: Gen (CI ByteString)
genHeaderName = CI.mk <$> genAscii

renderQueryBS :: Query -> ByteString
renderQueryBS [] = ""
renderQueryBS ql =
    let mkPair (name, value) = name <> maybe "" ("=" <>) value
    in "?" <> BS.intercalate "&" (map mkPair ql)

genQuery :: Gen Query
genQuery = listOf genQueryItem

genQueryItem :: Gen QueryItem
genQueryItem = (,) <$> genAscii <*> oneof [Just <$> genAscii, pure Nothing]

genAscii :: Gen ByteString
genAscii = BS.pack <$> do
    srange <- choose (3, 10)
    vectorOf srange $ oneof [choose ('a', 'z'), choose ('0', '9')]
