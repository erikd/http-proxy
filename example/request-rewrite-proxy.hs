{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Proxy

main :: IO ()
main = runProxySettings $ defaultSettings
                { proxyPort = 31081
                , proxyRequestModifier = noFacebook
                }


noFacebook :: Request -> IO Request
noFacebook req
 | serverName req == "www.facebook.com" =
         return $ req
                { serverName = "www.google.com"
                , serverPort = 80
                , pathInfo = []
                , queryString = []
                , rawQueryString = ""
                }

 | otherwise = return req
