{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

import Control.Monad.Trans.Resource
import Network.HTTP.Proxy

import Control.Concurrent (forkIO, killThread)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Conduit (($$))
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Conduit.Binary as BL
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT

import TestServer
import Util (headerShow)

testProxyPort, testServerPort :: Int
testProxyPort = 31081
testServerPort = 31080


main :: IO ()
main = runResourceT $ do
    -- Don't need to do anything with these ThreadIds
    _ <- with (forkIO $ runTestServer testServerPort) killThread
    _ <- with (forkIO $ runProxy testProxyPort) killThread
    runTests
    liftIO $ putStrLn "Tests complete."

--------------------------------------------------------------------------------

data Result = Result
    { status    :: Int
    , headers   :: ByteString
    , body      :: ByteString
    }
    deriving Eq

printResult :: Result -> IO ()
printResult Result{..} = do
    putStrLn $ "Status : " ++ show status
    putStrLn "Headers :"
    BS.putStr $ headers
    putStrLn "Body :"
    BS.putStrLn body


runTests :: ResourceT IO ()
runTests =
    testUrl $ "http://localhost:" ++ show testServerPort ++ "/"


testUrl :: String -> ResourceT IO ()
testUrl url = do
    request <- lift $ HC.parseUrl url
    direct <- httpRun request
    proxy <- httpRun $ HC.addProxy "localhost" testProxyPort request
    when (direct /= proxy) $ liftIO $ do
        printResult direct
        putStrLn "----------------------------------------------"
        printResult proxy
        putStrLn "Test failed."
        exitFailure


httpRun :: HC.Request IO -> ResourceT IO Result
httpRun req = liftIO $ HC.withManager $ \mgr -> do
    HC.Response st hdrs bdy <- HC.http req mgr
    bodyText <- bdy $$ BL.take 2048
    return $ Result (HT.statusCode st) (headerShow hdrs) $ BS.concat $ LBS.toChunks bodyText
