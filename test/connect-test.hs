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
import Control.Monad.IO.Class (liftIO)

import qualified Network.HTTP.Types as HT

import TestServer
import Util


debug :: Bool
debug = False

main :: IO ()
main = connectTest

connectTest :: IO ()
connectTest = runResourceT $ do
    printTestMsgR "HTTPS CONNECT test"
    -- Don't need to do anything with these ThreadIds
    _ <- with (forkIO $ runTestServerTLS testServerPort) killThread
    _ <- with (forkIO $ runProxySettings testProxySettings) killThread
    mapM_ (testSingleUrl debug) tests
    liftIO $ putStrLn "pass"
  where
    testProxySettings = defaultSettings
                    { proxyHost = "*6"
                    , proxyPort = testProxyPort
                    }
    tests =
        [ ( HT.methodGet,  "https://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show testServerPort ++ "/", Nothing )
        , ( HT.methodPost, "https://localhost:" ++ show testServerPort ++ "/", Just "Message\n" )
        , ( HT.methodGet,  "https://localhost:" ++ show testServerPort ++ "/forbidden", Nothing )
        ]

--------------------------------------------------------------------------------

