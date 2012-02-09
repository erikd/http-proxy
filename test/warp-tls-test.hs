{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Resource
import Network.TLS
import Network.Wai

import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Conduit (($$))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit.Binary as BL
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import TestServer
import Util


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

