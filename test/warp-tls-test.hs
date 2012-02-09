{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Resource
import Network.TLS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Blaze.ByteString.Builder (fromByteString)
import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Conduit (($$))
import Data.String (fromString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Binary as BL
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

testServerPort :: Int
testServerPort = 31080

main :: IO ()
main = runResourceT $ do
    _ <- with (forkIO $ runTestServerTLS testServerPort) killThread
    let url = "https://localhost:" ++ show testServerPort ++ "/"
    httpConduitUrl url

data Result = Result Int [HT.Header] ByteString


printResult :: Result -> IO ()
printResult (Result status headers body) = do
    putStrLn $ "Status : " ++ show status
    putStrLn "Headers :"
    BS.putStr $ headerShow headers
    putStrLn "Body :"
    BS.putStr body


httpConduitUrl :: String -> ResourceT IO ()
httpConduitUrl urlstr = do
    request <- lift $ HC.parseUrl urlstr
    direct <- httpRun request
    liftIO $ printResult direct

-- | Use HC.http to fullfil a HC.Request. We need to wrap it because the
-- Response contains a Source which we need to read to generate our result.
httpRun :: HC.Request IO -> ResourceT IO Result
httpRun req = do
    (_, mgr) <- withIO (HC.newManager manSettings) HC.closeManager
    HC.Response st hdrs bdy <- HC.http req mgr
    bodyText <- bdy $$ BL.take 8192
    return $ Result (HT.statusCode st) hdrs $ BS.concat $ LBS.toChunks bodyText
  where
    manSettings = HC.def { HC.managerCheckCerts = \ _ _ -> return CertificateUsageAccept }


runTestServerTLS :: Int -> IO ()
runTestServerTLS port =
    let settings = defaultSettings { settingsPort = port, settingsHost = "*6" }
        tlsSettings = TLSSettings "certificate.pem" "key.pem"
    in runTLS tlsSettings settings serverApp


serverApp :: Request -> ResourceT IO Response
serverApp req
 | rawPathInfo req == "/forbidden" = do
    let text = "This is the forbidden message.\n"
    let respHeaders =
            [ HT.headerContentType "text/plain"
            , HT.headerContentLength $ fromString $ show $ BS.length text
            ]
    return $ ResponseBuilder HT.statusForbidden respHeaders $ fromByteString text

 | otherwise = do
    let text = BS.concat
            [ "  Method          : " , requestMethod req , "\n"
            , "  HTTP Version    : " , fromString (show (httpVersion req)) , "\n"
            , "  Path Info       : " , rawPathInfo req , "\n"
            , "  Query String    : " , rawQueryString req , "\n"
            , "  Server Name     : " , serverName req , "\n"
            , "  Server Port     : " , fromString (show (serverPort req)), "\n"
            , "  Secure (SSL)    : " , fromString (show (isSecure req)), "\n"
            , "  Request Headers :\n\n"
            , headerShow (requestHeaders req)
            , "\n"
            ]
    let respHeaders =
            [ HT.headerContentType "text/plain"
            , HT.headerContentLength $ fromString $ show $ BS.length text
            ]
    return $ ResponseBuilder HT.statusOK respHeaders $ fromByteString text


headerShow :: [HT.Header] -> ByteString
headerShow headers =
    BS.concat $ map hShow headers
  where
    hShow (f, v) = BS.concat [ "  ", CI.original f , ": " , v, "\n" ]
