{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
--
-- Copyright     : Michael Snoyman, Stephen Blackheath, Erik de Castro Lopo
-- Maintainer    : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3
--
-- History:
--   This code originated in Michael Snoyman's Warp package when Warp was based
--   on the Data.Enumerator library. That code was modified by Stephen
--   Blackheath to turn it into a HTTP/HTTPS proxy and from there fell under the
--   maintainership of Erik de Castro Lopo who then did the conversion from
--   Data.Enumerator to Data.Conduit. During that conversion, Warp was again
--   used as a reference.
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
    ( runProxy
    , runProxySettings

    , Settings (..)
    , defaultSettings
    , UpstreamProxy (..)
    , Request (..)
    )
where

import Prelude hiding (catch, lines)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import Network    ( PortID(..) )
import Network.Socket
    ( accept, Family (..)
    , SocketType (Stream), listen, bindSocket, setSocketOption, maxListenQueue
    , SockAddr, SocketOption (ReuseAddr)
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    , Socket, sClose, HostName, ServiceName, socket, connect
    )
import Network.BSD ( getProtocolNumber )
import Network.Wai
import qualified Network.Socket
import qualified Network.Socket.ByteString as Sock
import Control.Applicative
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException, AsyncException (ThreadKilled)
    , bracketOnError, IOException, throw
    )
import Control.Concurrent (forkIO, killThread)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Network.HTTP.Conduit as HC

import Data.Typeable (Typeable)

import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteString)
import Control.Exception.Lifted (throwIO)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mempty)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Proxy.Timeout as T
import Data.Word (Word8)
import Data.List (delete, foldl')
import Control.Monad (forever, when, void)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPutStrLn, stderr)
import qualified Data.IORef as I
import Data.String (IsString (..))
import qualified Data.ByteString.Lex.Integral as LI
import Network.TLS (TLSCertificateUsage (..))
import qualified Data.ByteString.Base64 as B64

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

#if 0
import Data.Version (showVersion)
import qualified Paths_httpProxy

httpProxyVersion :: String
httpProxyVersion = showVersion Paths_warp.version
#endif

-- |
--
-- In order to provide slowloris protection, Warp provides timeout handlers. We
-- follow these rules:
--
-- * A timeout is created when a connection is opened.
--
-- * When all request headers are read, the timeout is tickled.
--
-- * Every time at least 2048 bytes of the request body are read, the timeout
--   is tickled.
--
-- * The timeout is paused while executing user code. This will apply to both
--   the application itself, and a ResponseSource response. The timeout is
--   resumed as soon as we return from user code.
--
-- * Every time data is successfully sent to the client, the timeout is tickled.
data Connection = Connection
    { connSendMany :: [B.ByteString] -> IO ()
    , connSendAll  :: B.ByteString -> IO ()
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> IO () -- ^ offset, length
    , connClose    :: IO ()
    , connRecv     :: IO B.ByteString
    }

socketConnection :: Socket -> Connection
socketConnection s = Connection
    { connSendMany = Sock.sendMany s
    , connSendAll = Sock.sendAll s
    , connSendFile = error "connSendFile : Cannot send a file here."
    , connClose = sClose s
    , connRecv = Sock.recv s bytesPerRead
    }


bindPort :: Int -> HostPreference -> IO Socket
bindPort p s = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE
                                         , AI_NUMERICSERV
                                         , AI_NUMERICHOST]
                             , addrSocketType = Stream }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> addrFamily x /= AF_INET6) addrs
        addrs6 = filter (\x -> addrFamily x == AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4 -> addrs4 ++ addrs6
                HostIPv6 -> addrs6 ++ addrs4
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) =
                                      catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"
        theBody addr =
          bracketOnError
          (Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          sClose
          (\sock -> do
              setSocketOption sock ReuseAddr 1
              bindSocket sock (addrAddress addr)
              listen sock maxListenQueue
              return sock
          )
    tryAddrs addrs'

-- | Run a HTTP and HTTPS proxy server on the specified port. This calls
-- 'runProxySettings' with 'defaultSettings'.
runProxy :: Port -> IO ()
runProxy p = runProxySettings defaultSettings { proxyPort = p }

-- | Run a HTTP and HTTPS proxy server with the specified settings.
runProxySettings :: Settings -> IO ()
#if WINDOWS
runProxySettings set = withSocketsDo $ do
    var <- MV.newMVar Nothing
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sClose s >> return Nothing
    _ <- forkIO $ bracket
        (bindPort (proxyPort set) (proxyHost set))
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            runSettingsSocket set s)
    forever (threadDelay maxBound) `finally` clean
#else
runProxySettings set =
    bracket
        (bindPort (proxyPort set) (proxyHost set))
        sClose
        (runSettingsSocket set)
#endif

type Port = Int


runSettingsSocket :: Settings -> Socket -> IO ()
runSettingsSocket set sock =
    runSettingsConnection set getter
  where
    getter = do
        (conn, sa) <- accept sock
        return (socketConnection conn, sa)

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> IO ()
runSettingsConnection set getConn = do
    let onE = proxyOnException set
        port = proxyPort set
    tm <- T.initialize $ proxyTimeout set * 1000000
    forever $ do
        (conn, addr) <- getConn
        _ <- forkIO $ do
            mgr <- HC.newManager managerSettingsNoCheck
            th <- T.registerKillThread tm
            serveConnection set th tm onE port conn addr mgr
            T.cancel th
            HC.closeManager mgr
        return ()

-- | Contains a @Source@ and a byte count that is still to be read in.
newtype IsolatedBSSource = IsolatedBSSource (I.IORef (Int, C.Source (ResourceT IO) ByteString))

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
ibsIsolate :: IsolatedBSSource -> C.Source (ResourceT IO) ByteString
ibsIsolate ibs@(IsolatedBSSource ref) =
    C.PipeM pull (return ())
  where
    pull = do
        (count, src) <- liftIO $ I.readIORef ref
        if count == 0
            -- No more bytes wanted downstream, so we're done.
            then return $ C.Done Nothing ()
            else do
                -- Get the next chunk (if available) and the updated source
                (src', mbs) <- src C.$$+ CL.head

                -- If no chunk available, then there aren't enough bytes in the
                -- stream. Throw a ConnectionClosedByPeer
                bs <- maybe (liftIO $ throwIO ConnectionClosedByPeer) return mbs

                let -- How many of the bytes in this chunk to send downstream
                    toSend = min count (S.length bs)
                    -- How many bytes will still remain to be sent downstream
                    count' = count - toSend
                case () of
                    ()
                        -- The expected count is greater than the size of the
                        -- chunk we just read. Send the entire chunk
                        -- downstream, and then loop on this function for the
                        -- next chunk.
                        | count' > 0 -> do
                            liftIO $ I.writeIORef ref (count', src')
                            return $ C.HaveOutput (ibsIsolate ibs) (return ()) bs

                        -- The expected count is the total size of the chunk we
                        -- just read. Send this chunk downstream, and then
                        -- terminate the stream.
                        | count == S.length bs -> do
                            liftIO $ I.writeIORef ref (count', src')
                            return $ C.HaveOutput (C.Done Nothing ()) (return ()) bs

                        -- Some of the bytes in this chunk should not be sent
                        -- downstream. Split up the chunk into the sent and
                        -- not-sent parts, add the not-sent parts onto the new
                        -- source, and send the rest of the chunk downstream.
                        | otherwise -> do
                            let (x, y) = S.splitAt toSend bs
                            liftIO $ I.writeIORef ref (count', C.HaveOutput src' (return ()) y)
                            return $ C.HaveOutput (C.Done Nothing ()) (return ()) x

-- | Extract the underlying @Source@ from an @IsolatedBSSource@, which will not
-- perform any more isolation.

serveConnection :: Settings
                -> T.Handle
                -> T.Manager
                -> (SomeException -> IO ())
                -> Port
                -> Connection -> SockAddr
                -> HC.Manager
                -> IO ()
serveConnection settings th tm onException port conn remoteHost' mgr =
    catch
        (finally
          (runResourceT serveConnection')
          (connClose conn))
        onException
  where
    serveConnection' :: ResourceT IO ()
    serveConnection' = do
        let fromClient = connSource conn th
        serveConnection'' fromClient

    serveConnection'' :: C.Source (ResourceT IO) ByteString -> ResourceT IO ()
    serveConnection'' fromClient = do
        (req, _ibs) <- parseRequest conn port remoteHost' fromClient
        case req of
            _ | requestMethod req `elem` [ "GET", "POST" ] ->
                    case lookup "host" (requestHeaders req) of
                        Nothing -> failRequest th conn req "Bad proxy request" ("Request '" `mappend` rawPathInfo req `mappend` "'.")
                                        >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
                        Just s -> do
                                let (hs, ps) = case S.split 58 s of -- ':'
                                        [h] -> (h, if isSecure req then 443 else 80)
                                        [h, p] -> (h, LI.readDecimal_ p)
                                        _ -> (serverName req, serverPort req)
                                modReq <- liftIO $ proxyRequestModifier settings req { serverName = hs, serverPort = ps }
                                proxyPlain (proxyUpstream settings) th conn mgr modReq
                                        >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
            _ | requestMethod req == "CONNECT" ->
                case B.split ':' (rawPathInfo req) of
                    [h, p] -> proxyConnect th tm conn h (LI.readDecimal_ p) req
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
                    _      -> failRequest th conn req "Bad request" ("Bad request '" `mappend` rawPathInfo req `mappend` "'.")
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
            _ ->
                failRequest th conn req "Unknown request" ("Unknown request '" `mappend` rawPathInfo req `mappend` "'.")
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient


parseRequest :: Connection -> Port -> SockAddr
             -> C.Source (ResourceT IO) S.ByteString
             -> ResourceT IO (Request, IsolatedBSSource)
parseRequest conn port remoteHost' src1 = do
    (src2, headers') <- src1 C.$$+ takeHeaders
    parseRequest' conn port headers' remoteHost' src2

-- FIXME come up with good values here
bytesPerRead, maxTotalHeaderLength :: Int
bytesPerRead = 4096
maxTotalHeaderLength = 50 * 1024

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
    | ConnectionClosedByPeer
    | OverLargeHeader
    deriving (Show, Typeable, Eq)
instance Exception InvalidRequest

handleExpect :: Connection
             -> H.HttpVersion
             -> ([H.Header] -> [H.Header])
             -> [H.Header]
             -> IO [H.Header]
handleExpect _ _ front [] = return $ front []
handleExpect conn hv front (("expect", "100-continue"):rest) = do
    connSendAll conn $
        if hv == H.http11
            then "HTTP/1.1 100 Continue\r\n\r\n"
            else "HTTP/1.0 100 Continue\r\n\r\n"
    return $ front rest
handleExpect conn hv front (x:xs) = handleExpect conn hv (front . (x:)) xs

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Connection
              -> Port
              -> [ByteString]
              -> SockAddr
              -> C.Source (ResourceT IO) S.ByteString -- FIXME was buffered
              -> ResourceT IO (Request, IsolatedBSSource)
parseRequest' _ _ [] _ _ = throwIO $ NotEnoughLines []
parseRequest' conn port (firstLine:otherLines) remoteHost' src = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath)
            | S.null rpath' = ("", "/")
            | "http://" `S.isPrefixOf` rpath' = S.breakByte 47 $ S.drop 7 rpath'
            | otherwise = ("", rpath')
    heads <- liftIO
           $ handleExpect conn httpversion id
             (map parseHeaderNoAttr otherLines)
    let host = fromMaybe host' $ lookup "host" heads
    let len0 =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs -> LI.readDecimal_ bs
    let serverName' = takeUntil 58 host -- ':'
    (rbody, ibs) <- liftIO $ do
        ibs <- fmap IsolatedBSSource $ I.newIORef (len0, src)
        return (ibsIsolate ibs, ibs)
    return (Request
            { requestMethod = method
            , httpVersion = httpversion
            , pathInfo = H.decodePathSegments rpath
            , rawPathInfo = rpath
            , rawQueryString = gets
            , queryString = H.parseQuery gets
            , serverName = serverName'
            , serverPort = port
            , requestHeaders = heads
            , isSecure = False
            , remoteHost = remoteHost'
            , requestBody = rbody
            , vault = mempty
            }, ibs)


takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> ResourceT IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
    case S.split 32 s of  -- ' '
        [method, query, http'] -> do
            let (hfirst, hsecond) = B.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (rpath, qstring) = S.breakByte 63 query  -- '?'
                        hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s
{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` copyByteString
                            (case httpversion of
                                H.HttpVersion 1 1 -> "1.1"
                                _ -> "1.0")
                `mappend` spaceBuilder
                `mappend` fromShow (H.statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (H.statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start (serverHeader responseHeaders)
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> H.Header -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` copyByteString (CI.original x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

checkPersist :: Request -> Bool
checkPersist req
    | ver == H.http11 = checkPersist11 conn
    | otherwise       = checkPersist10 conn
  where
    ver = httpVersion req
    conn = lookup "connection" $ requestHeaders req
    checkPersist11 (Just x)
        | CI.foldCase x == "close"      = False
    checkPersist11 _                    = True
    checkPersist10 (Just x)
        | CI.foldCase x == "keep-alive" = True
    checkPersist10 _                    = False

isChunked :: H.HttpVersion -> Bool
isChunked = (==) H.http11

hasBody :: H.Status -> Request -> Bool
hasBody s req = s /= H.Status 204 "" && s /= H.status304 &&
                H.statusCode s >= 200 && requestMethod req /= "HEAD"

sendResponse :: T.Handle
             -> Request -> Connection -> Response -> ResourceT IO Bool
sendResponse th req conn = sendResponse'
  where
    version = httpVersion req
    isPersist = checkPersist req
    isChunked' = isChunked version
    needsChunked hs = isChunked' && not (hasLength hs)
    isKeepAlive hs = isPersist && (isChunked' || hasLength hs)
    hasLength hs = isJust $ lookup "content-length" hs
    sendHeader = connSendMany conn . L.toChunks . toLazyByteString

    sendResponse' :: Response -> ResourceT IO Bool
    sendResponse' ResponseFile{} = error "Proxy cannot send a file."

    sendResponse' (ResponseBuilder s hs b)
        | hasBody s req = liftIO $ do
              toByteStringIO (\bs -> do
                connSendAll conn bs
                T.tickle th) body
              return (isKeepAlive hs)
        | otherwise = sendOtherwise headers'
      where
        headers' = headers version s hs
        needsChunked' = needsChunked hs
        body = if needsChunked'
                  then headers' needsChunked'
                       `mappend` chunkedTransferEncoding b
                       `mappend` chunkedTransferTerminator
                  else headers' False `mappend` b

    sendResponse' (ResponseSource s hs bodyFlush)
        | hasBody s req = do
            let src = CL.sourceList [headers' needsChunked'] `mappend`
                      (if needsChunked' then body C.$= chunk else body)
            src C.$$ builderToByteString C.=$ connSink conn th
            return $ isKeepAlive hs
        | otherwise = sendOtherwise headers'
      where
        body = fmap2 (\x -> case x of
                        C.Flush -> flush
                        C.Chunk builder -> builder) bodyFlush
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        needsChunked' = needsChunked hs
        chunk :: C.Conduit Builder (ResourceT IO) Builder
        chunk = C.NeedInput push close
        push x = C.HaveOutput chunk (return ()) (chunkedTransferEncoding x)
        close = C.HaveOutput (C.Done Nothing ()) (return ()) chunkedTransferTerminator

    sendOtherwise hdrs =  liftIO $ do
            sendHeader $ hdrs False
            T.tickle th
            return isPersist


fmap2 :: Functor m => (o1 -> o2) -> C.Pipe i o1 m r -> C.Pipe i o2 m r
fmap2 f (C.HaveOutput p c o) = C.HaveOutput (fmap2 f p) c (f o)
fmap2 f (C.NeedInput p c) = C.NeedInput (fmap2 f . p) (fmap2 f c)
fmap2 f (C.PipeM mp c) = C.PipeM (fmap (fmap2 f) mp) c
fmap2 _ (C.Done i x) = C.Done i x

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                   then SU.unsafeDrop 2 rest
                   else rest
     in (CI.mk k, rest')

connSource :: Connection -> T.Handle -> C.Source (ResourceT IO) ByteString
connSource Connection { connRecv = recv } th =
    src
  where
    src = C.PipeM (do
        bs <- liftIO recv
        if S.null bs
            then return $ C.Done Nothing ()
            else do
                when (S.length bs >= 2048) $ liftIO $ T.tickle th
                return (C.HaveOutput src (return ()) bs))
        (return ())

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> C.Sink B.ByteString (ResourceT IO) ()
connSink Connection { connSendAll = send } th =
    sink
  where
    sink = C.NeedInput push close
    close = liftIO (T.resume th)
    push x = C.PipeM (liftIO $ do
        T.resume th
        send x
        T.pause th
        return sink) (liftIO $ T.resume th)
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when Warp is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.

------ The functions below are not warp-specific and could be split out into a
--separate package.


-- | Various proxy server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { proxyPort = 3128 }
data Settings = Settings
    { proxyPort :: Int -- ^ Port to listen on. Default value: 3100
    , proxyHost :: HostPreference -- ^ Default value: HostIPv4
    , proxyOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , proxyTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , proxyRequestModifier :: Request -> IO Request -- ^ A function that allows the the request to be modified before being run. Default: 'return . id'.
    , proxyLogger :: ByteString -> IO () -- ^ A function for logging proxy internal state. Default: 'return ()'.
    , proxyUpstream :: Maybe UpstreamProxy -- ^ Optional upstream proxy.
    }

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @*6@ means @HostIPv6@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv6
  | Host String
    deriving (Show, Eq, Ord)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "*6" = HostIPv6
    -}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s = Host s

-- | A http-proxy can be configured to use and upstream proxy by providing the
-- proxy name, the port it listens to and an option username and password for
-- proxy authorisation.
data UpstreamProxy = UpstreamProxy
    { upstreamHost :: ByteString -- ^ The upstream proxy's hostname.
    , upstreamPort :: Int -- ^ The upstream proxy's port number.
    , upstreamAuth :: Maybe (ByteString, ByteString) -- ^ Optional username and password to use with upstream proxy.
    }

-- | The default settings for the Proxy server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { proxyPort = 3100
    , proxyHost = "*"
    , proxyOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing ->
                when (go' $ fromException e)
                    $ hPutStrLn stderr $ "ProxyEx: " ++ show e
    , proxyTimeout = 30
    , proxyRequestModifier = return . id
    , proxyLogger = \ _ -> return ()
    , proxyUpstream = Nothing
    }
  where
    go :: InvalidRequest -> IO ()
    go _ = return ()
    go' (Just ThreadKilled) = False
    go' _ = True

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

takeHeaders :: C.Sink ByteString (ResourceT IO) [ByteString]
takeHeaders =
    C.NeedInput (push (THStatus 0 id id)) close
  where
    close = throwIO IncompleteHeaders

    push (THStatus len lines prepend) bs
        -- Too many bytes
        | len > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise =
            case mnl of
                -- No newline find in this chunk.  Add it to the prepend,
                -- update the length, and continue processing.
                Nothing ->
                    let len' = len + bsLen
                        prepend' = prepend . S.append bs
                        status = THStatus len' lines prepend'
                     in C.NeedInput (push status) close
                -- Found a newline at position end.
                Just end ->
                    let start = end + 1 -- start of next chunk
                        line
                            -- There were some bytes before the newline, get them
                            | end > 0 = prepend $ SU.unsafeTake (checkCR bs end) bs
                            -- No bytes before the newline
                            | otherwise = prepend S.empty
                     in if S.null line
                            -- no more headers
                            then
                                let lines' = lines []
                                    -- leftover
                                    rest = if start < bsLen
                                               then Just (SU.unsafeDrop start bs)
                                               else Nothing
                                 in C.Done rest lines'
                            -- more headers
                            else
                                let len' = len + start
                                    lines' = lines . (line:)
                                    status = THStatus len' lines' id
                                 in if start < bsLen
                                        -- more bytes in this chunk, push again
                                        then let bs' = SU.unsafeDrop start bs
                                              in push status bs'
                                        -- no more bytes in this chunk, ask for more
                                        else C.NeedInput (push status) close
      where
        bsLen = S.length bs
        mnl = S.elemIndex 10 bs
{-# INLINE takeHeaders #-}

checkCR :: ByteString -> Int -> Int
checkCR bs pos =
  let !p = pos - 1
  in if '\r' == B.index bs p
        then p
        else pos
{-# INLINE checkCR #-}


serverHeader :: H.RequestHeaders -> H.RequestHeaders
serverHeader hdrs = case lookup key hdrs of
    Nothing  -> server : hdrs
    Just svr -> servers svr : delete (key,svr) hdrs
 where
    key = "Via"
    ver = B.pack "Proxy/0.0"
    server = (key, ver)
    servers svr = (key, S.concat [svr, " ", ver])

--------------------------------------------------------------------------------

proxyPlain :: Maybe UpstreamProxy -> T.Handle -> Connection -> HC.Manager -> Request -> ResourceT IO Bool
proxyPlain upstream th conn mgr req = do
        let portStr = case (serverPort req, isSecure req) of
                           (80, False) -> mempty
                           (443, True) -> mempty
                           (n, _) -> fromString (':' : show n)
            urlStr = (if isSecure req then "https://" else "http://")
                               `mappend` serverName req
                               `mappend` portStr
                               `mappend` rawPathInfo req
                               `mappend` rawQueryString req
            close =
                let hasClose hdrs = (== "close") . CI.mk <$> lookup "connection" hdrs
                    mClose = hasClose (requestHeaders req)
                    -- RFC2616: Connection defaults to Close in HTTP/1.0 and Keep-Alive in HTTP/1.1
                    defaultClose = httpVersion req == H.HttpVersion 1 0
                in  fromMaybe defaultClose mClose
            outHdrs = [(n,v) | (n,v) <- requestHeaders req, n `notElem` [ "Host", "Accept-Encoding", "Content-Length" ]]
        -- liftIO $ putStrLn $ B.unpack (requestMethod req) ++ " " ++ B.unpack urlStr
        let contentLength = if requestMethod req == "GET"
             then 0
             else LI.readDecimal_ . fromMaybe "0" . lookup "content-length" . requestHeaders $ req

        let (proxy, pauth) = case upstream of
                                 Nothing -> (Nothing, [])
                                 Just (UpstreamProxy ph pp Nothing) ->
                                         ( Just (HC.Proxy ph pp), [])
                                 Just (UpstreamProxy ph pp (Just (u, p))) ->
                                         ( Just (HC.Proxy ph pp)
                                         , [ ( "Proxy-Authorization"
                                             , B.append "Basic " (B64.encode $ B.concat [ u, ":", p ])
                                             )
                                           ] )

        url <-
            (\u -> u { HC.method = requestMethod req
                     , HC.requestHeaders = pauth ++ outHdrs
                     , HC.rawBody = True
                     , HC.secure = isSecure req
                     , HC.requestBody = HC.RequestBodySource contentLength
                                      $ fmap2 copyByteString
                                      $ requestBody req
                     , HC.proxy = proxy
                     -- In a proxy we do not want to intercept non-2XX status codes.
                     , HC.checkStatus = \ _ _ -> Nothing
                     })
                <$> lift (HC.parseUrl (B.unpack urlStr))

        HC.Response sc hver rh bodySource <- HC.http url mgr
        close' <- handleHttpReply close sc hver rh
        bodySource C.$$ connSink conn th
        return $ not close'
      where
        handleHttpReply close status hversion hdrs = do
            let remoteClose = isNothing ("content-length" `lookup` hdrs)
                close' = close || remoteClose
                hdrs' = [(n, v) | (n, v) <- hdrs, n `notElem`
                             ["connection", "proxy-connection"]
                        ]
                          ++ [("Connection", if close' then "Close" else "Keep-Alive")]
            liftIO $ connSendMany conn $ L.toChunks $ toLazyByteString $
                            headers hversion status hdrs' False
            return remoteClose

failRequest :: T.Handle -> Connection -> Request -> ByteString -> ByteString -> ResourceT IO Bool
failRequest th conn req headerMsg bodyMsg =
    sendResponse th req conn $ ResponseBuilder status hdrs $ copyByteString bodyMsg
  where
    hdrs = [("Content-Length", B.pack . show . B.length $ bodyMsg)]
    status = H.status500 { H.statusMessage = headerMsg }


proxyConnect :: T.Handle -> T.Manager -> Connection -> ByteString -> Int -> Request -> ResourceT IO Bool
proxyConnect th tm conn host prt req = do
        -- liftIO $ putStrLn $ B.unpack (requestMethod req) ++ " " ++ B.unpack host ++ ":" ++ show prt
        eConn <- liftIO $ do
            usock <- socketConnection `fmap` connectTo (B.unpack host) (PortNumber . fromIntegral $ prt)
            return $ Right usock
          `catch` \(exc :: IOException) ->
            return $ Left $ "Unable to connect: " `mappend` B.pack (show exc)
        case eConn of
            Right uconn -> do
                liftIO $
                    connSendMany conn $ L.toChunks $ toLazyByteString
                                      $ headers (httpVersion req) statusConnectOK [] False
                void $ allocate (forkIO $ do
                            wrTh <- T.registerKillThread tm
                            runResourceT (connSource uconn th C.$$ connSink conn wrTh)
                            T.cancel wrTh
                        ) killThread
                connSource conn th C.$$ connSink uconn th
                return False
            Left errorMsg ->
                failRequest th conn req errorMsg ("PROXY FAILURE\r\n" `mappend` errorMsg)

statusConnectOK :: H.Status
statusConnectOK = H.Status 200 "Connection established"

connectTo :: HostName   -- Hostname
          -> PortID     -- Port Identifier
          -> IO Socket  -- Connected Socket
connectTo hostname (Service serv) = connect' hostname serv
connectTo hostname (PortNumber port) = connect' hostname (show port)
connectTo _ (UnixSocket _) = error "Cannot connect to a UnixSocket"

connect' :: HostName -> ServiceName -> IO Socket
connect' host serv = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
    firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    sClose  -- only done if there's an error
    (\sock -> do
        connect sock (addrAddress addr)
        return sock
        )

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
firstSuccessful :: [IO a] -> IO a
firstSuccessful [] = error "firstSuccessful: empty list"
firstSuccessful (p:ps) = catch p $ \e ->
    case ps of
        [] -> throw (e :: IOException)
        _  -> firstSuccessful ps


managerSettingsNoCheck :: HC.ManagerSettings
managerSettingsNoCheck =
    HC.def { HC.managerCheckCerts = \ _ _ -> return CertificateUsageAccept }
