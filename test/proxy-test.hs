{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

import Control.Monad.Trans.Resource
import Network.HTTP.Proxy

import Control.Concurrent (forkIO, killThread)
import Control.Monad (forM_, unless, when)
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
import Util (headerShow)

testProxyPort, testServerPort :: Int
testProxyPort = 31081
testServerPort = 31080

debug :: Bool
debug = False

main :: IO ()
main = runResourceT $ do
    -- Don't need to do anything with these ThreadIds
    _ <- with (forkIO $ runTestServer testServerPort) killThread
    _ <- with (forkIO $ runProxy testProxyPort) killThread
    runTests
    liftIO $ putStrLn "Tests complete."

--------------------------------------------------------------------------------

type TestRequest = ( HT.Method, String, Maybe ByteString )

data Result = Result Int [HT.Header] ByteString


printResult :: Result -> IO ()
printResult (Result status headers body) = do
    putStrLn $ "Status : " ++ show status
    putStrLn "Headers :"
    BS.putStr $ headerShow headers
    putStrLn "Body :"
    BS.putStrLn body


runTests :: ResourceT IO ()
runTests = mapM_ testUrl
    [ ( HT.methodGet,  "http://localhost:" ++ show testServerPort ++ "/", Nothing )
    , ( HT.methodPost, "http://localhost:" ++ show testServerPort ++ "/", Nothing )
    , ( HT.methodPost, "http://localhost:" ++ show testServerPort ++ "/", Just "Message\n" )
    , ( HT.methodGet,  "http://localhost:" ++ show testServerPort ++ "/forbidden", Nothing )
    ]


testUrl :: TestRequest -> ResourceT IO ()
testUrl testreq = do
    request <- lift $ setupRequest testreq
    direct <- httpRun request
    proxy <- httpRun $ HC.addProxy "localhost" testProxyPort request
    when debug $ liftIO $ do
        printResult direct
        printResult proxy
    compareResult direct proxy


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
    assert b msg = unless b $ error msg
