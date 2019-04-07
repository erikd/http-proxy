{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS

import           Network.HTTP.Proxy (Settings (..), Request (..))
import qualified Network.HTTP.Proxy as Proxy
import           Network.URI (URI (..), URIAuth (..), parseURI)
import           Network.Wai.Internal (Response)

main :: IO ()
main =
  Proxy.runProxySettings $
    Proxy.defaultProxySettings
      { proxyPort = 31081
      , proxyHttpRequestModifier = secureGoogle
      }

-- Modifying the request like this is only possible for unencrypted HTTP connections
-- by my be useful for eg redirecting HTTP to HTTPS.
-- HTTPS cnnections cannot be modified like this because the for HTTPS connections
-- even the request itself is encrypted.

secureGoogle :: Request -> IO (Either Response Request)
secureGoogle req = do
  case parseURI $ BS.unpack (requestPath req) of
    Nothing -> do
      putStrLn $ "Not able to parse: " ++ show (requestPath req)
      -- Not much to be done other than just return the Request unmodified.
      pure $ Right req
    Just uri ->
      pure . Right $ req { requestPath = BS.pack $ show (modifyURI uri) }

modifyURI :: URI -> URI
modifyURI uri =
  uri
    { uriAuthority = modifyUriAthority <$> uriAuthority uri
    , uriScheme = modifyUriScheme (uriScheme uri)
    }
 where
  modifyUriAthority :: URIAuth -> URIAuth
  modifyUriAthority auth =
    if uriRegName auth == "www.google.com"
      then auth { uriRegName = "encrypted.google.com", uriPort = "" }
      else auth

  modifyUriScheme :: String -> String
  modifyUriScheme scheme =
    if scheme =="http:" then "https:" else scheme
