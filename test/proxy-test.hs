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


import TestServer


main :: IO ()
main = runResourceT $ do
    -- Don't need to do anything with these ThreadIds
    _ <- with (forkIO $ runTestServer 31080) killThread
    _ <- with (forkIO $ runProxy 31081) killThread
    runTests


runTests :: ResourceT IO ()
runTests = return ()
