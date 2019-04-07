{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Proxy (ProxySettings (..), Request (..))
import qualified Network.HTTP.Proxy as Proxy

main :: IO ()
main =
  Proxy.runProxySettings $
    Proxy.defaultProxySettings
      { proxyPort = 31081
      , proxyHttpRequestModifier = Just secureGoogle
      }

-- Modifying the request like this is only possible for unencrypted HTTP connections
-- by my be useful for eg redirecting HTTP to HTTPS.
-- HTTPS cnnections cannot be modified like this because the for HTTPS connections
-- even the request itself is encrypted.

secureGoogle :: Request -> IO Request
secureGoogle req
  | "www.google.com" `BS.isInfixOf` requestPath req
      && not ("https" `BS.isprefixOf` requestPath req =
        pure $ req
                { requestPath = "encrypted.google.com"
                }

  | otherwise = pure req
