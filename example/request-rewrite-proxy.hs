{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Proxy

main :: IO ()
main = runProxySettings $ defaultProxySettings
                { proxyPort = 31081
                , proxyRequestModifier = Just secureGoogle
                }

-- We can modify the request so that instead of going to unsecured Google
-- search page, people get redirected to the encrypted version.

secureGoogle :: Request -> IO Request
secureGoogle req
    | requestHost req == "www.google.com" =
        return $ req
                { requestHost = "encrypted.google.com"
                , requestPort = 443
                }

    | otherwise = return req
