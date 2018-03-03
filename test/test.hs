{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------
-- Copyright : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
------------------------------------------------------------

import Test.Hspec (Spec, describe, hspec)
import Test.Hspec.QuickCheck (prop)

import Network.HTTP.Proxy.Request

import Test.Gen
import Test.QuickCheck

import Test.Wai



main :: IO ()
main =
  hspec requestTest

-- Test that a Request can be pulled apart and reconstructed without losing
-- anything.
requestTest :: Spec
requestTest = describe "Request:" $
  prop "Roundtrips with waiRequest." $ forAll genWaiRequest $ \wreq ->
    wreq `waiShouldBe` (waiRequest wreq . proxyRequest) wreq
