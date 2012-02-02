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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Conduit (($$))

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

data Result = Result Int [HT.Header] ByteString


printResult :: Result -> IO ()
printResult (Result status headers body) = do
    putStrLn $ "Status : " ++ show status
    putStrLn "Headers :"
    BS.putStr $ headerShow headers
    putStrLn "Body :"
    BS.putStrLn body


runTests :: ResourceT IO ()
runTests = do
    testUrl $ "http://localhost:" ++ show testServerPort ++ "/"
    testUrl $ "http://www.mega-nerd.com/index.html"


testUrl :: String -> ResourceT IO ()
testUrl url = do
    request <- lift $ HC.parseUrl url
    direct <- httpRun request
    proxy <- httpRun $ HC.addProxy "localhost" testProxyPort request
    when False $ liftIO $ do
        printResult direct
        printResult proxy
    compareResult direct proxy


-- | Use HC.http to fullfil a HC.Request. We need to wrap it because the
-- Response contains a Source which was need to read to generate our result.
httpRun :: HC.Request IO -> ResourceT IO Result
httpRun req = liftIO $ HC.withManager $ \mgr -> do
    HC.Response st hdrs bdy <- HC.http req mgr
    bodyText <- bdy $$ BL.take 8192
    return $ Result (HT.statusCode st) hdrs $ BS.concat $ LBS.toChunks bodyText


-- | Compare results and error out if they're different.
compareResult :: Result -> Result -> ResourceT IO ()
compareResult (Result sa ha ba) (Result sb hb bb) = liftIO $ do
    assert (sa == sb) $ "HTTP status codes don't match : " ++ show sa ++ " /= " ++ show sb
    forM_ [ "server", "content-type", "content-length" ] $ \v ->
        let xa = lookup v ha
            xb = lookup v hb
        in assert (xa == xb) $ "Header field '" ++ show v ++ "' doesn't match : '" ++ show xa ++ "' /= '" ++ show xb
    assert (ba == bb) $ "HTTP response bodies are different :\n" ++ BS.unpack ba ++ "\n-----------\n" ++ BS.unpack bb
  where
    assert :: Bool -> String -> IO ()
    assert b msg = when (not b) $ error msg
