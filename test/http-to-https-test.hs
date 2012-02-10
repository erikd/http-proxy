{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

import Control.Monad.Trans.Resource
import Network.HTTP.Proxy
import Network.TLS

import Control.Concurrent (forkIO, killThread)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Conduit (($$))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit.Binary as BL
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import TestServer
import Util

testProxyPort, testServerPort :: Int
testProxyPort = 31081
testServerPort = 31080

debug :: Bool
debug = False

main :: IO ()
main = httpToHtppsRewriteTest


httpToHtppsRewriteTest :: IO ()
httpToHtppsRewriteTest = runResourceT $ do
    printTestMsgR "Rewrite HTTP to HTTPS"

    -- Don't need to do anything with these ThreadIds
    _ <- with (forkIO $ runTestServerTLS testServerPort) killThread
    _ <- with (forkIO $ runProxySettings proxySettings) killThread
    request <- lift $ setupRequest
        ( HT.methodGet,  "https://localhost:" ++ show testServerPort ++ "/", Nothing )
    direct <- httpRun request
    proxy <- httpRun $ HC.addProxy "localhost" testProxyPort $ request { HC.secure = False }
    when debug $ liftIO $ do
        printResult direct
        printResult proxy
    compareResult direct proxy
    liftIO $ putStrLn "pass"


proxySettings :: Settings
proxySettings = defaultSettings
                { proxyPort = testProxyPort
                , proxyHost = "*6"
                , proxyRequestModifier = httpsRedirector
                }


httpsRedirector :: Request -> IO Request
httpsRedirector req
 | serverName req == "localhost"
    && not (isSecure req) = do
         return $ req
                { isSecure = True
                , serverPort = testServerPort
                }

 | otherwise = return req

--------------------------------------------------------------------------------


setupRequest :: Monad m => TestRequest -> IO (HC.Request m)
setupRequest (method, url, reqBody) = do
    req <- HC.parseUrl url
    return $ req
        { HC.method = if HC.method req /= method then method else HC.method req
        , HC.requestBody = case reqBody of
                            Just x -> HC.RequestBodyBS x
                            Nothing -> HC.requestBody req
        -- In this test program we want to pass error pages back to the test
        -- function so the error output can be compared.
        , HC.checkStatus = \ _ _ -> Nothing
        }


-- | Use HC.http to fullfil a HC.Request. We need to wrap it because the
-- Response contains a Source which was need to read to generate our result.
httpRun :: HC.Request IO -> ResourceT IO Result
httpRun req = liftIO $ withManagerSettings settings $ \mgr -> do
    HC.Response st hdrs bdy <- HC.http req mgr
    bodyText <- bdy $$ BL.take 8192
    return $ Result (HT.statusCode st) hdrs $ BS.concat $ LBS.toChunks bodyText
  where
    settings = HC.def { HC.managerCheckCerts = \ _ _ -> return CertificateUsageAccept }

