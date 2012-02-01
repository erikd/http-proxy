{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module Util where

import Network.HTTP.Types

import Data.ByteString                    (ByteString)

import qualified Data.ByteString          as BS
import qualified Data.CaseInsensitive     as CI

headerShow :: [Header] -> ByteString
headerShow headers =
    BS.concat $ map hShow headers
  where
    hShow (f, v) = BS.concat [ "  ", CI.original f , ": " , v, "\n" ]
