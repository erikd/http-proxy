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

import Prelude hiding (
# if __GLASGOW_HASKELL__ < 0706
    catch,
#endif
    lines)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import Network    ( PortID(..) )
import Network.Socket
    ( accept
    , SocketType (Stream)
    , SockAddr
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    , Socket, sClose, HostName, ServiceName, socket, connect
    )
import Network.BSD ( getProtocolNumber )
import Network.Wai
import qualified Network.Socket.ByteString as Sock
import Control.Applicative
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException, AsyncException (ThreadKilled)
    , bracketOnError, IOException, throw
    )
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe, isJust)
import Data.Char (toLower, isHexDigit)
import Data.Word (Word)

import Data.Typeable (Typeable)

import Data.Conduit
import Data.Conduit.Internal (ResumableSource (..))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Blaze (builderToByteString)
import Control.Exception.Lifted (throwIO)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mempty)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Proxy.Timeout as T
import Data.Word (Word8)
import Data.List (delete, foldl')
import Control.Monad (forever, when, void)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (stderr)
import qualified Data.IORef as I
import Data.Conduit.Network (bindPort, HostPreference)
import Data.String (IsString (..))
import qualified Data.ByteString.Lex.Integral as LI
import Network.TLS (CertificateUsage (..))
import qualified Data.ByteString.Base64 as B64
import qualified Network.HTTP.Conduit as HC
import Data.Maybe (isNothing)
import Control.Concurrent (killThread)
import Control.Monad.Trans.Resource (allocate)
import System.IO (hPutStrLn)

#if WINDOWS
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
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO () -- ^ offset, length
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
newtype IsolatedBSSource = IsolatedBSSource (I.IORef (Int, ResumableSource (ResourceT IO) ByteString))

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
ibsIsolate :: IsolatedBSSource -> Source (ResourceT IO) ByteString
ibsIsolate ibs@(IsolatedBSSource ref) = do
    (count, src) <- liftIO $ I.readIORef ref
    if count == 0
        -- No more bytes wanted downstream, so we're done.
        then return ()
        else do
            -- Get the next chunk (if available) and the updated source
            (src', mbs) <- lift $ src $$++ CL.head

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
                        yield bs
                        ibsIsolate ibs

                    -- The expected count is the total size of the chunk we
                    -- just read. Send this chunk downstream, and then
                    -- terminate the stream.
                    | count == S.length bs -> do
                        liftIO $ I.writeIORef ref (count', src')
                        yield bs

                    -- Some of the bytes in this chunk should not be sent
                    -- downstream. Split up the chunk into the sent and
                    -- not-sent parts, add the not-sent parts onto the new
                    -- source, and send the rest of the chunk downstream.
                    | otherwise -> do
                        let (x, y) = S.splitAt toSend bs
                        liftIO $ I.writeIORef ref (count', fmapResume (yield y >>) src')
                        yield x

-- | Extract the underlying @Source@ from an @IsolatedBSSource@, which will not
-- perform any more isolation.
ibsDone :: IsolatedBSSource -> IO (ResumableSource (ResourceT IO) ByteString)
ibsDone (IsolatedBSSource ref) = fmap snd $ I.readIORef ref

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

    serveConnection'' :: Source (ResourceT IO) ByteString -> ResourceT IO ()
    serveConnection'' fromClient = do
        (req, _ibs) <- parseRequest conn port remoteHost' fromClient
        case req of
            _ | requestMethod req `elem` [ "GET", "POST" ] -> do
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
             -> Source (ResourceT IO) S.ByteString
             -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest conn port remoteHost' src1 = do
    (src2, headers') <- src1 $$+ takeHeaders
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
              -> ResumableSource (ResourceT IO) S.ByteString -- FIXME was buffered
              -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
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
    let chunked = maybe False ((== "chunked") . B.map toLower)
                  $ lookup "transfer-encoding" heads
    (rbody, getSource) <- liftIO $
        if chunked
          then do
            ref <- I.newIORef (src, NeedLen)
            return (chunkedSource ref, fmap fst $ I.readIORef ref)
          else do
            ibs <- fmap IsolatedBSSource $ I.newIORef (len0, src)
            return (ibsIsolate ibs, ibsDone ibs)

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
            , requestBodyLength = ChunkedBody
            , vault = mempty
            }, getSource)

data ChunkState = NeedLen
                | NeedLenNewline
                | HaveLen Word

chunkedSource :: MonadIO m
              => I.IORef (ResumableSource m ByteString, ChunkState)
              -> Source m ByteString
chunkedSource ipair = do
    (src, mlen) <- liftIO $ I.readIORef ipair
    go src mlen
  where
    go' src front = do
        (src', (len, bs)) <- lift $ src $$++ front getLen
        let src''
                | S.null bs = src'
                | otherwise = fmapResume (yield bs >>) src'
        go src'' $ HaveLen len

    go src NeedLen = go' src id
    go src NeedLenNewline = go' src (CB.take 2 >>)
    go src (HaveLen 0) = liftIO $ I.writeIORef ipair (src, HaveLen 0)
    go src (HaveLen len) = do
        (src', mbs) <- lift $ src $$++ CL.head
        case mbs of
            Nothing -> liftIO $ I.writeIORef ipair (src', HaveLen 0)
            Just bs ->
                case S.length bs `compare` fromIntegral len of
                    EQ -> yield' src' NeedLenNewline bs
                    LT -> do
                        let mlen = HaveLen $ len - fromIntegral (S.length bs)
                        yield' src' mlen bs
                    GT -> do
                        let (x, y) = S.splitAt (fromIntegral len) bs
                        let src'' = fmapResume (yield y >>) src'
                        yield' src'' NeedLenNewline x

    yield' src mlen bs = do
        liftIO $ I.writeIORef ipair (src, mlen)
        yield bs
        go src mlen

    getLen :: Monad m => Sink ByteString m (Word, ByteString)
    getLen = do
        mbs <- CL.head
        case mbs of
            Nothing -> return (0, S.empty)
            Just bs -> do
                (x, y) <-
                    case S.breakByte 10 bs of
                        (x, y)
                            | S.null y -> do
                                mbs2 <- CL.head
                                case mbs2 of
                                    Nothing -> return (x, y)
                                    Just bs2 -> return $ S.breakByte 10 $ bs `S.append` bs2
                            | otherwise -> return (x, y)
                let w =
                        S.foldl' (\i c -> i * 16 + fromIntegral (hexToWord c)) 0
                        $ B.takeWhile isHexDigit x
                return (w, S.drop 1 y)

    hexToWord w
        | w < 58 = w - 48
        | w < 71 = w - 55
        | otherwise = w - 87

takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> ResourceT IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
    case filter (not . S.null) $ S.splitWith (\c -> c == 32 || c == 9) s of  -- ' '
        (method:query:http'') -> do
            let http' = S.concat http''
                (hfirst, hsecond) = B.splitAt 5 http'
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
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
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
                      (if needsChunked' then body $= chunk else body)
            src $$ builderToByteString =$ connSink conn th
            return $ isKeepAlive hs
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
      where
        body = mapOutput (\x -> case x of
                        Flush -> flush
                        Chunk builder -> builder) bodyFlush
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        needsChunked' = needsChunked hs
        chunk :: Conduit Builder (ResourceT IO) Builder
        chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')

connSource :: Connection -> T.Handle -> Source (ResourceT IO) ByteString
connSource Connection { connRecv = recv } th =
    src
  where
    src = do
        bs <- liftIO recv
        if S.null bs
            then return ()
            else do
                when (S.length bs >= 2048) $ liftIO $ T.tickle th
                yield bs
                src

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> Sink B.ByteString (ResourceT IO) ()
connSink Connection { connSendAll = send } th =
    sink
  where
    sink = await >>= maybe close push
    close = liftIO (T.resume th)
    push x = do
        liftIO $ T.resume th
        liftIO $ send x
        liftIO $ T.pause th
        sink
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

takeHeaders :: Sink ByteString (ResourceT IO) [ByteString]
takeHeaders =
    await >>= maybe (throwIO ConnectionClosedByPeer) (push (THStatus 0 id id))
  where
    close :: Sink ByteString (ResourceT IO) a
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
                     in await >>= maybe close (push status)
                -- Found a newline, but next line continues as a multiline header
                Just (end, True) ->
                    let rest = S.drop (end + 1) bs
                        prepend' = prepend . S.append (SU.unsafeTake (checkCR bs end) bs)
                        len' = len + end
                        status = THStatus len' lines prepend'
                     in push status rest
                -- Found a newline at position end.
                Just (end, False) ->
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
                                 in maybe (return ()) leftover rest >> return lines'
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
                                        else await >>= maybe close (push status)
      where
        bsLen = S.length bs
        mnl = do
            nl <- S.elemIndex 10 bs
            -- check if there are two more bytes in the bs
            -- if so, see if the second of those is a horizontal space
            if bsLen > nl + 1
                then
                    let c = S.index bs (nl + 1)
                     in Just (nl, c == 32 || c == 9)
                else Just (nl, False)
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

fmapResume :: (Source m o1 -> Source m o2) -> ResumableSource m o1 -> ResumableSource m o2
fmapResume f (ResumableSource src m) = ResumableSource (f src) m

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
                                      $ mapOutput copyByteString
                                      $ requestBody req
                     , HC.proxy = proxy
                     -- In a proxy we do not want to intercept non-2XX status codes.
                     , HC.checkStatus = \ _ _ _ -> Nothing
                     })
                <$> lift (HC.parseUrl (B.unpack urlStr))

        -- HC.Response sc hver rh bodyResSource
        resp <- HC.http url mgr
        close' <- handleHttpReply close (HC.responseStatus resp) (HC.responseVersion resp) (HC.responseHeaders resp)
        (bodySource, _) <- unwrapResumable $ HC.responseBody resp
        bodySource $$ connSink conn th
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
                            runResourceT (connSource uconn th $$ connSink conn wrTh)
                            T.cancel wrTh
                        ) killThread
                connSource conn th $$ connSink uconn th
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
    HC.def { HC.managerCheckCerts = \ _ _ _ -> return CertificateUsageAccept }
