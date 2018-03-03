{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

module Test.Wai where

import Network.Wai.Internal
import Test.Hspec

waiShouldBe :: Request -> Request -> Expectation
waiShouldBe a b = do
    requestMethod a         `shouldBe` requestMethod b
    httpVersion a           `shouldBe` httpVersion b
    rawPathInfo a           `shouldBe` rawPathInfo b
    rawQueryString a        `shouldBe` rawQueryString b
    requestHeaders a        `shouldBe` requestHeaders b
    isSecure a              `shouldBe` isSecure b
    remoteHost a            `shouldBe` remoteHost b
    pathInfo a              `shouldBe` pathInfo b
    queryString a           `shouldBe` queryString b
    -- requestBody a
    -- vault a                 `shouldBe` vault b
    -- requestBodyLength a     `shouldBe` requestBodyLength b
    requestHeaderHost a     `shouldBe` requestHeaderHost b
    requestHeaderRange a    `shouldBe` requestHeaderRange b


