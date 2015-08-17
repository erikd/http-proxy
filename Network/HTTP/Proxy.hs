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

    , runProxy
    , runProxySettings
    , defaultSettings
    )
    where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (Flush (..), Sink, Source, ($$), mapOutput, yield)
import Data.Conduit.Network
import Data.Monoid
import Network.Wai.Conduit hiding (Request)

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
-- 'runProxySettings' with 'defaultSettings'.
runProxy :: Port -> IO ()
runProxy port = runProxySettings $ defaultSettings { proxyPort = port }

-- | Run a HTTP and HTTPS proxy server with the specified settings.
runProxySettings :: Settings -> IO ()
runProxySettings set =
    HC.newManager HC.tlsManagerSettings
        >>= Warp.runSettings (warpSettings set) . proxyApp set


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
    , proxyRequestModifier :: Request -> IO Request -- ^ A function that allows the the request to be modified before being run. Default: 'return'.
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
    , proxyRequestModifier = return
    , proxyLogger = const $ return ()
    , proxyUpstream = Nothing
    }
  where
    defaultExceptionResponse :: SomeException -> Wai.Response
    defaultExceptionResponse e =
        Wai.responseLBS HT.internalServerError500
                [ (HT.hContentType, "text/plain; charset=utf-8") ]
                $ LBS.fromChunks [BS.pack $ show e]

-- -----------------------------------------------------------------------------

proxyApp :: Settings -> HC.Manager -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
proxyApp settings mgr wreq respond
    | Wai.requestMethod wreq == "CONNECT" =
            respond $ responseRawSource (handleConnect wreq)
                    (Wai.responseLBS HT.status500 [("Content-Type", "text/plain")] "No support for responseRaw")
    | otherwise = do
        wreq' <- waiRequest <$> proxyRequestModifier settings (proxyRequest wreq)
        hreq0 <- HC.parseUrl $ BS.unpack (Wai.rawPathInfo wreq' <> Wai.rawQueryString wreq')
        let hreq = hreq0
                { HC.method = Wai.requestMethod wreq'
                , HC.requestHeaders = filter dropRequestHeader $ Wai.requestHeaders wreq'
                , HC.redirectCount = 0 -- Always pass redirects back to the client.
                , HC.requestBody =
                    case Wai.requestBodyLength wreq of
                        Wai.ChunkedBody ->
                            HC.requestBodySourceChunkedIO (sourceRequestBody wreq)
                        Wai.KnownLength l ->
                            HC.requestBodySourceIO (fromIntegral l) (sourceRequestBody wreq)
                , HC.decompress = const True
                , HC.checkStatus = \_ _ _ -> Nothing
                }
        HC.withResponse hreq mgr $ \res -> do
            let body = mapOutput (Chunk . fromByteString) $ HCC.bodyReaderSource $ HC.responseBody res
                headers = (CI.mk "X-Via-Proxy", "yes") : filter dropResponseHeader (HC.responseHeaders res)
            respond $ responseSource (HC.responseStatus res) headers body
      where
        dropRequestHeader (k, _) = k `notElem`
            [ "content-encoding"
            , "content-length"
            ]
        dropResponseHeader (k, _) = k `notElem` []


handleConnect :: Wai.Request -> Source IO BS.ByteString -> Sink BS.ByteString IO () -> IO ()
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
        yield "HTTP/1.1 200 OK\r\n\r\n" $$ toClient
        race_
            (fromClient $$ NC.appSink ad)
            (NC.appSource ad $$ toClient)
