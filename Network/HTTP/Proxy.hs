{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Copyright  : Michael Snoyman, Erik de Castro Lopo
-- Maintainer : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License : BSD3
--
-- History:
--   Previous versions of http-proxy included a modified version of the Warp
--   web server. Thankfully, Michael Snoyman made changes to Warp and Wai to
--   allow both a HTTP and a HTTPS proxy to be implemented solely as a Wai
--   Application.
--   This version of http-proxy is based on a piece of code Michael Snoyman
--   published as a gist on github.com.
--
--------------------------------------------------------------------------------

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

    , httpProxyApp
    , warpSettings

    , runProxy
    , runProxySettings
    , runProxySettingsSocket
    , defaultProxySettings
    )
    where

import Data.ByteString.Builder (byteString)
import Control.Concurrent.Async (race_)
import Control.Exception -- (SomeException, catch, toException)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (ConduitT, Flush (..), (.|), mapOutput, runConduit, yield)
import Data.Conduit.Network
#if ! MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Void (Void)
import Network.Socket
import Network.Wai.Conduit hiding (Request, requestMethod)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Network as NC
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Client.Conduit as HCC
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
-- 'runProxySettings' with 'defaultProxySettings'.
runProxy :: Port -> IO ()
runProxy port = runProxySettings $ defaultProxySettings { proxyPort = port }

-- | Run a HTTP and HTTPS proxy server with the specified settings.
runProxySettings :: Settings -> IO ()
runProxySettings set = do
    mgr <- HC.newManager HC.tlsManagerSettings
    Warp.runSettings (warpSettings set) $ httpProxyApp set mgr

-- | Run a HTTP and HTTPS proxy server with the specified settings but provide
-- it with a Socket to accept connections on. The Socket should have already
-- have had `bind` and `listen` called on it so that the proxy can simple
-- `accept` connections.
runProxySettingsSocket :: Settings -> Socket -> IO ()
runProxySettingsSocket set sock = do
    port <- socketPort sock
    mgr <- HC.newManager HC.tlsManagerSettings
    Warp.runSettingsSocket (warpSettings set) sock
            $ httpProxyApp set { proxyPort = fromIntegral port } mgr

-- | Various proxy server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultProxySettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultProxySettings { proxyPort = 3128 }
data Settings = Settings
    { proxyPort :: Int
      -- ^ Port to listen on. Default value: 3100
    , proxyHost :: HostPreference
    -- ^ Default value: HostIPv4
    , proxyOnException :: SomeException -> Wai.Response
    -- ^ What to do with exceptions thrown by either the application or server.
    -- Default: ignore server-generated exceptions (see 'InvalidRequest') and print
    -- application-generated applications to stderr.
    , proxyTimeout :: Int
    -- ^ Timeout value in seconds. Default value: 30
    , proxyHttpRequestModifier :: Request -> IO (Either Response Request)
    -- ^ A function that allows the request to be modified before being run. Default: 'return . Right'.
    -- This only works for unencrypted HTTP requests (eg to upgrade the request to HTTPS) because
    -- HTTPS requests are encrypted.
    , proxyLogger :: ByteString -> IO ()
    -- ^ A function for logging proxy internal state. Default: 'return ()'.
    , proxyUpstream :: Maybe UpstreamProxy
    -- ^ Optional upstream proxy.
    }

-- | A http-proxy can be configured to use and upstream proxy by providing the
-- proxy name, the port it listens to and an option username and password for
-- proxy authorisation.
data UpstreamProxy = UpstreamProxy
    { upstreamHost :: ByteString
    -- ^ The upstream proxy's hostname.
    , upstreamPort :: Int
    -- ^ The upstream proxy's port number.
    , upstreamAuth :: Maybe (ByteString, ByteString)
    -- ^ Optional username and password to use with upstream proxy.
    }


warpSettings :: Settings -> Warp.Settings
warpSettings pset = Warp.setPort (proxyPort pset)
    . Warp.setHost (proxyHost pset)
    . Warp.setTimeout (proxyTimeout pset)
    . Warp.setOnException (\ _ _ -> return ())
    . Warp.setOnExceptionResponse defaultExceptionResponse
    $ Warp.setNoParsePath True Warp.defaultSettings

-- | The default settings for the Proxy server. See the individual settings for
-- the default value.
defaultProxySettings :: Settings
defaultProxySettings = Settings
    { proxyPort = 3100
    , proxyHost = "*"
    , proxyOnException = defaultExceptionResponse
    , proxyTimeout = 30
    , proxyHttpRequestModifier = return . Right
    , proxyLogger = const $ return ()
    , proxyUpstream = Nothing
    }

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
    Wai.responseLBS HT.internalServerError500
        [ (HT.hContentType, "text/plain; charset=utf-8") ]
        $ LBS.fromChunks [BS.pack $ show e]

-- -----------------------------------------------------------------------------
-- Application == Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

httpProxyApp :: Settings -> HC.Manager -> Application
httpProxyApp settings mgr wreq respond = do
    mwreq <- proxyHttpRequestModifier settings $ proxyRequest wreq
    either respond (doUpstreamRequest settings mgr respond . waiRequest wreq) mwreq


doUpstreamRequest :: Settings -> HC.Manager -> (Wai.Response -> IO Wai.ResponseReceived) -> Wai.Request -> IO Wai.ResponseReceived
doUpstreamRequest settings mgr respond mwreq
    | Wai.requestMethod mwreq == "CONNECT" =
        respond $ responseRawSource (handleConnect mwreq)
                    (Wai.responseLBS HT.status500 [("Content-Type", "text/plain")] "No support for responseRaw")
    | otherwise = do
        hreq0 <- HC.parseRequest $ BS.unpack (Wai.rawPathInfo mwreq <> Wai.rawQueryString mwreq)
        let hreq = hreq0
                { HC.method = Wai.requestMethod mwreq
                , HC.requestHeaders = filter dropRequestHeader $ Wai.requestHeaders mwreq
                , HC.redirectCount = 0 -- Always pass redirects back to the client.
                , HC.requestBody =
                    case Wai.requestBodyLength mwreq of
                        Wai.ChunkedBody ->
                            HC.requestBodySourceChunkedIO (sourceRequestBody mwreq)
                        Wai.KnownLength l ->
                            HC.requestBodySourceIO (fromIntegral l) (sourceRequestBody mwreq)
                -- Do not touch response body. Otherwise there may be discrepancy
                -- between response headers and the response content.
                , HC.decompress = const False
                }
        handle (respond . errorResponse) $
            HC.withResponse hreq mgr $ \res -> do
                let body = mapOutput (Chunk . byteString) . HCC.bodyReaderSource $ HC.responseBody res
                    headers = (CI.mk "X-Via-Proxy", "yes") : filter dropResponseHeader (HC.responseHeaders res)
                respond $ responseSource (HC.responseStatus res) headers body
      where
        dropRequestHeader (k, _) = k `notElem`
            [ "content-encoding"
            , "content-length"
            ]
        dropResponseHeader (k, _) = k `notElem` []

        errorResponse :: SomeException -> Wai.Response
        errorResponse = proxyOnException settings . toException


handleConnect :: Wai.Request -> ConduitT () ByteString IO () -> ConduitT ByteString Void IO a -> IO ()
handleConnect wreq fromClient toClient = do
    let (host, port) =
            case BS.break (== ':') $ Wai.rawPathInfo wreq of
                (x, "") -> (x, 80)
                (x, y) ->
                    case BS.readInt $ BS.drop 1 y of
                        Just (port', _) -> (x, port')
                        Nothing -> (x, 80)
        settings = clientSettings port host
    runTCPClient settings $ \ad -> do
        _ <- runConduit $ yield "HTTP/1.1 200 OK\r\n\r\n" .| toClient
        race_
            (runConduit $ fromClient .| NC.appSink ad)
            (runConduit $ NC.appSource ad .| toClient)
