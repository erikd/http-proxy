{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}

---------------------------------------------------------
--
-- Copyright     : Michael Snoyman, Erik de Castro Lopo
-- Maintainer    : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3
--
-- History:
--   Previous versions of http-proxy included a modified version of the Warp
--   web server. Thankfully, Michael Snoyman made changes to Warp and Wai to
--   allow both a HTTP and a HTTPS proxyi to be implemented solely as a Wai
--   Application.
--   This version of http-proxy is based on a piece of code Michael Snoyman
--   published as a gist on github.com.
--
---------------------------------------------------------

-- | This module contains a simple HTTP and HTTPS proxy. In the most basic
-- setup, the caller specifies a port and runs it as follows:
--
-- > -- Run a HTTPS and HTTPS proxy on port 3128.
-- > import Network.HTTP.Proxy
-- >
-- > main :: IO ()
-- > main = runProxy 3128
-- >
--

module Network.HTTP.Proxy
    ( Port
    , Request (..)
    , Settings (..)
    , UpstreamProxy (..)

    , runProxy
    , runProxySettings
    , defaultSettings
    )
    where

import Blaze.ByteString.Builder (fromByteString)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Network (HostPreference)
import Data.Monoid

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Network as NC
import qualified Network.HTTP.Client.Conduit as HC
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Network.HTTP.Proxy.Request

#if 0
import Data.Version (showVersion)
import qualified Paths_httpProxy

httpProxyVersion :: String
httpProxyVersion = showVersion Paths_warp.version
#endif


-- | Run a HTTP and HTTPS proxy server on the specified port. This calls
-- 'runProxySettings' with 'defaultSettings'.
runProxy :: Port -> IO ()
runProxy port = runProxySettings $ defaultSettings { proxyPort = port }

-- | Run a HTTP and HTTPS proxy server with the specified settings.
runProxySettings :: Settings -> IO ()
runProxySettings set = Warp.runSettings (warpSettings set) proxyAppWrapper


-- | Various proxy server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { proxyPort = 3128 }
data Settings = Settings
    { proxyPort :: Int -- ^ Port to listen on. Default value: 3100
    , proxyHost :: HostPreference -- ^ Default value: HostIPv4
    , proxyOnException :: SomeException -> Wai.Response -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , proxyTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , proxyRequestModifier :: Maybe (Request -> IO Request) -- ^ A function that allows the the request to be modified before being run. Default: 'return . id'.
    , proxyLogger :: ByteString -> IO () -- ^ A function for logging proxy internal state. Default: 'return ()'.
    , proxyUpstream :: Maybe UpstreamProxy -- ^ Optional upstream proxy.
    }

-- | A http-proxy can be configured to use and upstream proxy by providing the
-- proxy name, the port it listens to and an option username and password for
-- proxy authorisation.
data UpstreamProxy = UpstreamProxy
    { upstreamHost :: ByteString -- ^ The upstream proxy's hostname.
    , upstreamPort :: Int -- ^ The upstream proxy's port number.
    , upstreamAuth :: Maybe (ByteString, ByteString) -- ^ Optional username and password to use with upstream proxy.
    }


warpSettings :: Settings -> Warp.Settings
warpSettings pset = Warp.setPort (proxyPort pset)
    . Warp.setHost (proxyHost pset)
    . Warp.setTimeout (proxyTimeout pset)
    $ Warp.setNoParsePath True Warp.defaultSettings

-- | The default settings for the Proxy server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { proxyPort = 3100
    , proxyHost = "*"
    , proxyOnException = defaultExceptionResponse
    , proxyTimeout = 30
    , proxyRequestModifier = Nothing
    , proxyLogger = \ _ -> return ()
    , proxyUpstream = Nothing
    }
  where
    defaultExceptionResponse :: SomeException -> Wai.Response
    defaultExceptionResponse _ =
        Wai.responseLBS HT.internalServerError500
                [(HT.hContentType, "text/plain; charset=utf-8")]
                "Something went wrong in the proxy."


-- class ReaderT Manager m a

proxyAppWrapper :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
proxyAppWrapper wreq respond = runResourceT $ proxyApp wreq  respond


proxyApp :: (MonadBaseControl IO m, MonadReader env m, HC.HasHttpManager env) => Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> m Wai.ResponseReceived
proxyApp wreq respond
    | null $ Wai.requestHeaders wreq = failRequest "Error empty request headers." wreq >>= respond

    | Wai.isSecure wreq = failRequest "Secure connection to proxy?" wreq >>= respond

    | Wai.requestMethod wreq == "CONNECT" = connectApp wreq >>= respond

    | otherwise = do
            hreq <- httpConduitRequest wreq
            HC.withManager $ HC.withResponse hreq $ undefined respond
            {-
            HC.withResponse hreq $ \ hresp -> liftIO $ respond $
                Wai.responseStream (HC.responseStatus hresp) (cleanResponseHeaders hresp) undefined
            -}

{-
    | otherwise = do
        hreq <- liftIO $ httpConduitRequest wreq
        HC.withResponse hreq $ \hresp ->
            respond $ Wai.responseStream (HC.responseStatus hresp) (cleanResponseHeaders hresp) $ \write flush -> undefined

                -- liftIO $ await >>= maybe (return ()) (\x -> write (fromByteString x) >> flush)
                        {-
                        HC.responseBody hreps $ \body
                        write $ fromByteString "Hello\n"
                        flush
                        write $ fromByteString "World\n"
                        -}

        {-
        Wai.responseSourceBracket (HC.responseOpen hreq manager) (HC.responseClose) $ \res -> do
            let body = mapOutput (Chunk . fromByteString) $ HC.bodyReaderSource $ HC.responseBody res
                headers = filter safeResHeader $ HC.responseHeaders res
            return (HC.responseStatus res, headers, body)
        -}
-}


wibbler :: (Wai.Response -> IO Wai.ResponseReceived)
            -> HC.Response (ConduitM i0 ByteString n0 ()) -> m Wai.ResponseReceived
wibbler = undefined

{-
wibbler :: MonadIO m => (Response (ConduitM i ByteString n ()) -> m a) -> m a Wai.Response
wibbler hreq =
    HC.withResponse hreq $ \ hresp ->
        Wai.responseStream (HC.responseStatus hresp) (cleanResponseHeaders hresp) $ undefined
-}

cleanResponseHeaders :: HC.Response body -> [(HT.HeaderName, ByteString)]
cleanResponseHeaders =
    filter safeResHeader . HC.responseHeaders
  where
    safeResHeader (k, _) = k `notElem` [] -- FIXME expand

httpConduitRequest :: Wai.Request -> IO HC.Request
httpConduitRequest wreq = do
        let url = Wai.rawPathInfo wreq <> Wai.rawQueryString wreq
        hreq <- HC.parseUrl $ BS.unpack url
        return $ hreq
                { HC.method = Wai.requestMethod wreq
                , HC.requestHeaders = filter safeReqHeader $ Wai.requestHeaders wreq
                , HC.requestBody =
                    case Wai.requestBodyLength wreq of
                        Wai.ChunkedBody -> undefined -- HC.requestBodySourceChunkedIO (Wai.requestBody wreq)
                        Wai.KnownLength l -> undefined l -- HC.requestBodySourceIO (fromIntegral l) (Wai.requestBody wreq)
                , HC.decompress = \_ -> True
                , HC.checkStatus = \_ _ _ -> Nothing
                }
      where
        safeReqHeader (k, _) = k `elem` -- FIXME expand
            [ "user-agent"
            , "accept"
            , "cookie"
            ]

--------------------------------------------------------------------------------

failRequest :: Monad m => ByteString -> Wai.Request -> m Wai.Response
failRequest msg wreq = do
    let err =
            [ msg
            , "\n"
            , waiRequestHost wreq
            , Wai.rawPathInfo wreq
            , Wai.rawQueryString wreq
            , "\n"
            ]
    let respHeaders =
            [ ( HT.hContentType, "text/plain" )
            , ( HT.hContentLength, BS.pack . show . sum $ map BS.length err)
            ]
    return $ Wai.responseLBS HT.status400 respHeaders $ LBS.fromChunks err

--------------------------------------------------------------------------------

connectApp = undefined

{-
connectApp :: Wai.Request -> IO Wai.Response
connectApp wreq =
    return $ Wai.responseRaw (sslConnect wreq)
            $ Wai.responseLBS HT.status500 [("Content-Type", "text/plain")] "No support for responseRaw"

sslConnect :: Wai.Request -> Source IO BS.ByteString -> Sink BS.ByteString IO () -> IO ()
sslConnect wreq fromClient toClient = do
    let (host, port) =
            case BS.break (== ':') $ Wai.rawPathInfo wreq of
                (x, "") -> (x, 80)
                (x, y) ->
                    case BS.readInt $ BS.drop 1 y of
                        Just (port', _) -> (x, port')
                        Nothing -> (x, 80)
        settings = NC.clientSettings port host
    NC.runTCPClient settings $ \ad -> do
        yield "HTTP/1.1 200 OK\r\n\r\n" $$ toClient
        race_ (fromClient $$ NC.appSink ad) (NC.appSource ad $$ toClient)
-}

--------------------------------------------------------------------------------

waiRequestHost :: Wai.Request -> ByteString
waiRequestHost req = maybe "???" id $ lookup (CI.mk "Host") (Wai.requestHeaders req)

{-

requestPort :: Wai.Request -> Port
requestPort = snd . requestHostPort

requestHostPort :: Wai.Request -> (ByteString, Port)
requestHostPort = undefined
-}
