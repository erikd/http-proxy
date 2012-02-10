{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright (c)  Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
---------------------------------------------------------

module Util where

import Control.Monad.Trans.Resource
import Network.TLS
import System.IO

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit (($$))
import Data.Int (Int64)
import Data.String (fromString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai


type TestRequest = ( HT.Method, String, Maybe ByteString )


testProxyPort, testServerPort :: Int
testProxyPort = 31081
testServerPort = 31080



dumpWaiRequest :: Wai.Request -> IO ()
dumpWaiRequest req =
    let text = BS.concat
            [ "------- Wai Request --------------------------------------------------------------\n"
            , "Method          : " , Wai.requestMethod req , "\n"
            , "HTTP Version    : " , fromString (show (Wai.httpVersion req)) , "\n"
            , "Path Info       : " , Wai.rawPathInfo req , "\n"
            , "Query String    : " , Wai.rawQueryString req , "\n"
            , "Server Name     : " , Wai.serverName req , "\n"
            , "Server Port     : " , fromString (show (Wai.serverPort req)), "\n"
            , "Secure (SSL)    : " , fromString (show (Wai.isSecure req)), "\n"
            , "Remote Host     : " , fromString (show (Wai.remoteHost req)), "\n"
            , "Request Headers :\n"
            , headerShow (Wai.requestHeaders req), "\n"
            ]
    in BS.putStr text


dumpHttpConduitRequest :: HC.Request m -> IO ()
dumpHttpConduitRequest req =
    let text = BS.concat
            [ "------- HttpConduit Request ------------------------------------------------------\n"
            , "Method          : " , HC.method req , "\n"
            , "Secure (SSL)    : " , fromString (show (HC.secure req)), "\n"
            , "Host Name       : " , HC.host req , "\n"
            , "Host Port       : " , fromString (show (HC.port req)), "\n"
            , "Path            : " , HC.path req , "\n"
            , "Query String    : " , HT.urlDecode False (HC.queryString req), "\n"
            , "Request Headers :\n"
            , headerShow (HC.requestHeaders req), "\n"
            ]
    in BS.putStr text


dumpHttpResponse :: HT.Status -> HT.ResponseHeaders -> IO ()
dumpHttpResponse s rh = do
    let text = BS.concat
            [ "------- Response from upsteam ----------------------------------------------------\n"
            , "HTTP/1.0 ", BS.pack (show (HT.statusCode s)), " ", HT.statusMessage s, "\n"
            ]

    BS.putStr text
    BS.putStr $ BS.concat $ map (\ (f, v) -> BS.concat [ "    ", CI.original f, ": ", v, "\n" ]) rh


headerShow :: [HT.Header] -> ByteString
headerShow headers =
    BS.concat $ map hdrShow headers
  where
    hdrShow (f, v) = BS.concat [ "  ", CI.original f , ": " , v, "\n" ]

--------------------------------------------------------------------------------

data Result = Result Int [HT.Header] ByteString


printResult :: Result -> IO ()
printResult (Result status headers body) = do
    putStrLn $ "Status : " ++ show status
    putStrLn "Headers :"
    BS.putStr $ headerShow headers
    putStrLn "Body :"
    BS.putStrLn body

--------------------------------------------------------------------------------

printTestMsgR :: String -> ResourceT IO ()
printTestMsgR str = liftIO $ do
    putStr $ "    " ++ (take 45 (str ++ replicate 45 ' ')) ++ ": "
    hFlush stdout

opSizeMsgR :: String -> Int64 -> ResourceT IO ()
opSizeMsgR op size =
    printTestMsgR $ "Testing " ++ op ++ " operation  (" ++ show size ++ " bytes)"

--------------------------------------------------------------------------------

withManagerSettings :: ResourceIO m => HC.ManagerSettings -> (HC.Manager -> ResourceT m a) -> m a
withManagerSettings settings f = runResourceT $ do
    (_, manager) <- withIO (HC.newManager settings) HC.closeManager
    f manager

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

testSingleUrl :: Bool -> TestRequest -> ResourceT IO ()
testSingleUrl debug testreq = do
    request <- liftIO $ setupRequest testreq
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
httpRun req = liftIO $ withManagerSettings settings $ \mgr -> do
    HC.Response st hdrs bdy <- HC.http req mgr
    bodyText <- bdy $$ CB.take 8192
    return $ Result (HT.statusCode st) hdrs $ BS.concat $ LBS.toChunks bodyText
  where
    settings = HC.def { HC.managerCheckCerts = \ _ _ -> return CertificateUsageAccept }



