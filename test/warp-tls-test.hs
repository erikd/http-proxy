{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Resource
import Network.TLS

import Control.Concurrent (forkIO, killThread)
import Control.Monad (unless, when)
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


testServerPort :: Int
testServerPort = 31080

debug :: Bool
debug = False

main :: IO ()
main = warpTlsTest

warpTlsTest :: IO ()
warpTlsTest = runResourceT $ do
    printTestMsgR "Test Warp with TLS"
    _ <- with (forkIO $ runTestServerTLS testServerPort) killThread
    let url = "https://localhost:" ++ show testServerPort ++ "/"
    request <- lift $ HC.parseUrl url
    direct@(Result _ hdrs _) <- httpRun request
    let isWarp =
            case lookup "server" hdrs of
                Just s -> BS.isPrefixOf "Warp" s
                Nothing -> False
    unless isWarp $ error "No 'Server: Warp' header."
    when debug $ liftIO $ printResult direct
    liftIO $ putStrLn "passed"


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

