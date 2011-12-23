{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Proxy

main :: IO ()
main = runProxySettings $ defaultSettings
                { proxyPort = 31081
                , proxyRequestModifier = secureGoogle
                }

-- We can modify the request so that instead of going to unsecured Google
-- search page, people get redirected to the encrypted version.

secureGoogle :: Request -> IO Request
secureGoogle req
 | serverName req == "www.google.com" =
         return $ req
                { isSecure = True
                , serverName = "encrypted.google.com"
                , serverPort = 443
                , pathInfo = []
                , queryString = []
                , rawQueryString = ""
                }

 | otherwise = return req
