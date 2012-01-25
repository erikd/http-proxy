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
--   on the Data.Source library. That code was modified by Stephen Blackheath
--   to turn it into a HTTP/HTTP proxy and from there fell under the maintainership
--   of Erik de Castro Lopo who then did the conversion from Data.Enumerator to
--   Data.Conduit. During that conversion, Warp was again used as a reference.
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

import Control.Monad.Trans.Resource (ResourceT, runResourceT, with)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteString)
import Control.Exception.Lifted (throwIO)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO)
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
import Network.HTTP.Proxy.ReadInt (readInt64)

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


bindPort :: Int -> String -> IO Socket
bindPort p s = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE
                                         , AI_NUMERICSERV
                                         , AI_NUMERICHOST]
                             , addrSocketType = Stream }
        host = if s == "*" then Nothing else Just s
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = if null addrs' then head addrs else head addrs'
    bracketOnError
        (Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        sClose
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )

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
    mgr <- HC.newManager HC.def
    forever $ do
        (conn, addr) <- getConn
        _ <- forkIO $ do
            th <- T.registerKillThread tm
            serveConnection set th tm onE port conn addr mgr
            T.cancel th
        return ()

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
        fromClient <- C.bufferSource $ C.Source $ return $ connSource conn th
        serveConnection'' fromClient

    serveConnection'' fromClient = do
        req <- parseRequest port remoteHost' fromClient
        --liftIO $ print $ requestHeaders req
        case req of
            _ | requestMethod req `elem` [ "GET", "POST" ] ->
                liftIO (proxyRequestModifier settings req)
                        >>= proxyPlain th conn mgr
                        >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
            _ | requestMethod req == "CONNECT" ->
                case B.split ':' (rawPathInfo req) of
                    [h, p] -> proxyConnect th tm conn h (readInt p) req
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
                    _      -> failRequest th conn req "Bad request" ("Bad request '" `mappend` rawPathInfo req `mappend` "'.")
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient
            _ ->
                failRequest th conn req "Unknown request" ("Unknown request '" `mappend` rawPathInfo req `mappend` "'.")
                                >>= \keepAlive -> when keepAlive $ serveConnection'' fromClient

parseRequest :: Port -> SockAddr
             -> C.BufferedSource IO S.ByteString
             -> ResourceT IO Request
parseRequest port remoteHost' src = do
    headers' <- src C.$$ takeHeaders
    parseRequest' port headers' remoteHost' src

-- FIXME come up with good values here
bytesPerRead, maxTotalHeaderLength :: Int
bytesPerRead = 4096
maxTotalHeaderLength = 50 * 1024

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
    | OverLargeHeader
    deriving (Show, Typeable, Eq)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> SockAddr
              -> C.BufferedSource IO S.ByteString
              -> ResourceT IO Request
parseRequest' _ [] _ _ = throwIO $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' src = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath)
            | S.null rpath' = ("", "/")
            | "http://" `S.isPrefixOf` rpath' = S.breakByte 47 $ S.drop 7 rpath'
            | otherwise = ("", rpath')
    let heads = map parseHeaderNoAttr otherLines
    let host = fromMaybe host' $ lookup "host" heads
    let len =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs -> readInt bs
    let serverName' = takeUntil 58 host -- ':'
    -- FIXME isolate takes an Integer instead of Int or Int64. If this is a
    -- performance penalty, we may need our own version.
    rbody <- C.prepareSource $
        if len == 0
            then mempty
            else src C.$= CB.isolate len
    return Request
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
            , requestBody = C.Source $ return rbody
            , vault = mempty
            }


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
sendResponse th req conn r = sendResponse' r
  where
    version = httpVersion req
    isPersist = checkPersist req
    isChunked' = isChunked version
    needsChunked hs = isChunked' && not (hasLength hs)
    isKeepAlive hs = isPersist && (isChunked' || hasLength hs)
    hasLength hs = isJust $ lookup "content-length" hs

    sendResponse' :: Response -> ResourceT IO Bool
    sendResponse' ResponseFile{} = error "Proxy cannot send a file."

    sendResponse' (ResponseBuilder s hs b)
        | hasBody s req = liftIO $ do
              toByteStringIO (\bs -> do
                connSendAll conn bs
                T.tickle th) body
              return (isKeepAlive hs)
        | otherwise = liftIO $ do
            connSendMany conn
                $ L.toChunks
                $ toLazyByteString
                $ headers' False
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

    sendResponse' (ResponseSource s hs body) =
        response
      where
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        response
            | not (hasBody s req) = liftIO $ do
                connSendMany conn
                   $ L.toChunks $ toLazyByteString
                   $ headers' False
                T.tickle th
                return (checkPersist req)
            | otherwise = do
                let src =
                        CL.sourceList [headers' needsChunked'] `mappend`
                        (if needsChunked' then body C.$= chunk else body)
                src C.$$ builderToByteString C.=$ connSink conn th
                return $ isKeepAlive hs
        needsChunked' = needsChunked hs
        chunk :: C.Conduit Builder IO Builder
        chunk = C.Conduit $ return C.PreparedConduit
            { C.conduitPush = push
            , C.conduitClose = close
            }

        push x = return $ C.Producing [chunkedTransferEncoding x]
        close = return [chunkedTransferTerminator]

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                   then SU.unsafeDrop 2 rest
                   else rest
     in (CI.mk k, rest')

connSource :: Connection -> T.Handle -> C.PreparedSource IO ByteString
connSource Connection { connRecv = recv } th = C.PreparedSource
    { C.sourcePull = do
        bs <- liftIO recv
        if S.null bs
            then return C.Closed
            else do
                when (S.length bs >= 2048) $ liftIO $ T.tickle th
                return (C.Open bs)
    , C.sourceClose = return ()
    }

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> C.Sink B.ByteString IO ()
connSink Connection { connSendAll = send } th = C.Sink $ return C.SinkData
    { C.sinkPush = push
    , C.sinkClose = close
    }
  where
    close = liftIO (T.resume th)
    push x = do
        liftIO $ T.resume th
        liftIO $ send x
        liftIO $ T.pause th
        return C.Processing
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
    , proxyHost :: String -- ^ Host to bind to, or * for all. Default value: *
    , proxyOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , proxyTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , proxyRequestModifier :: Request -> IO Request -- ^ A function that allows the the request to be modified before being run. Default: 'return . id'.
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
                    $ hPutStrLn stderr $ show e
    , proxyTimeout = 30
    , proxyRequestModifier = return . id
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

takeHeaders :: C.Sink ByteString IO [ByteString]
takeHeaders =
    C.sinkState (THStatus 0 id id) takeHeadersPush close
  where
    close _ = throwIO IncompleteHeaders
{-# INLINE takeHeaders #-}

takeHeadersPush :: THStatus
                -> ByteString
                -> ResourceT IO (THStatus, C.SinkResult ByteString [ByteString])
takeHeadersPush (THStatus len _ _ ) _
    | len > maxTotalHeaderLength = throwIO OverLargeHeader
takeHeadersPush (THStatus len lines prepend) bs =
    case mnl of
        -- no newline.  prepend entire bs to next line
        Nothing -> do
            let len' = len + bsLen
            return (THStatus len' lines (prepend . S.append bs), C.Processing)
        Just nl -> do
            let end = nl
                start = nl + 1
                line = prepend (if end > 0
                                    then SU.unsafeTake (checkCR bs end) bs
                                    else S.empty)
            if S.null line
                -- no more headers
                then do
                    let lines' = lines []
                    if start < bsLen
                        then do
                            let rest = SU.unsafeDrop start bs
                            return (undefined, C.Done (Just rest) lines')
                        else return (undefined, C.Done Nothing lines')
                -- more headers
                else do
                    let len' = len + start
                        lines' = lines . (:) line
                    if start < bsLen
                        then do
                            let more = SU.unsafeDrop start bs
                            takeHeadersPush (THStatus len' lines' id) more
                        else return (THStatus len' lines' id, C.Processing)
  where
    bsLen = S.length bs
    mnl = S.elemIndex 10 bs
{-# INLINE takeHeadersPush #-}

checkCR :: ByteString -> Int -> Int
checkCR bs pos =
  let !p = pos - 1
  in if '\r' == B.index bs p
        then p
        else pos
{-# INLINE checkCR #-}

readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs
{-# INLINE readInt #-}


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

proxyPlain :: T.Handle -> Connection -> HC.Manager -> Request -> ResourceT IO Bool
proxyPlain th conn mgr req = do
        let urlStr = "http://" `mappend` serverName req
                               `mappend` rawPathInfo req
                               `mappend` rawQueryString req
            close =
                let hasClose hdrs = (== "close") . CI.mk <$> lookup "connection" hdrs
                    mClose = hasClose (requestHeaders req)
                    -- RFC2616: Connection defaults to Close in HTTP/1.0 and Keep-Alive in HTTP/1.1
                    defaultClose = httpVersion req == H.HttpVersion 1 0
                in  fromMaybe defaultClose mClose
            outHdrs = [(n,v) | (n,v) <- requestHeaders req, n /= "Host"]
        liftIO $ putStrLn $ B.unpack (requestMethod req) ++ " " ++ B.unpack urlStr
        let contentLength = if requestMethod req == "GET"
             then 0
             else readInt . fromMaybe "0" . lookup "content-length" . requestHeaders $ req

        url <-
            (\u -> u { HC.method = requestMethod req,
                       HC.requestHeaders = outHdrs,
                       HC.rawBody = True,
                       HC.requestBody = HC.RequestBodySource contentLength
                                            $ fmap copyByteString
                                            $ requestBody req
                                            })
                <$> lift (HC.parseUrl (B.unpack urlStr))

        HC.Response sc rh bodySource <- HC.http url mgr
        close' <- handleHttpReply close sc rh
        bodySource C.$$ connSink conn th
        return $ not close'
      where
        handleHttpReply close status hdrs = do
            let remoteClose = isNothing ("content-length" `lookup` hdrs)
                close' = close || remoteClose
                hdrs' = [(n, v) | (n, v) <- hdrs, n `notElem`
                             ["connection", "proxy-connection"]
                        ]
                          ++ [("Connection", if close' then "Close" else "Keep-Alive")]
            liftIO $ connSendMany conn $ L.toChunks $ toLazyByteString $
                            headers (httpVersion req) status hdrs' False
            return remoteClose

failRequest :: T.Handle -> Connection -> Request -> ByteString -> ByteString -> ResourceT IO Bool
failRequest th conn req headerMsg bodyMsg =
    sendResponse th req conn $ ResponseBuilder status hdrs $ copyByteString bodyMsg
  where
    hdrs = [("Content-Length", B.pack . show . B.length $ bodyMsg)]
    status = H.status500 { H.statusMessage = headerMsg }


proxyConnect :: T.Handle -> T.Manager -> Connection -> ByteString -> Int -> Request -> ResourceT IO Bool
proxyConnect th tm conn host prt req = do
        liftIO $ putStrLn $ B.unpack (requestMethod req) ++ " " ++ B.unpack host ++ ":" ++ show prt
        eConn <- liftIO $ do
            usock <- socketConnection `fmap` connectTo (B.unpack host) (PortNumber . fromIntegral $ prt)
            return $ Right usock
          `catch` \(exc :: IOException) ->
            return $ Left $ "Unable to connect: " `mappend` B.pack (show exc)
        case eConn of
            Right uconn -> do
                fromUpstream <- C.bufferSource $ C.Source $ return $ connSource uconn th
                liftIO $
                    connSendMany conn $ L.toChunks $ toLazyByteString
                                      $ headers (httpVersion req) H.statusOK [] False
                void $ with (forkIO $ do
                            wrTh <- T.registerKillThread tm
                            runResourceT (fromUpstream C.$$ connSink conn wrTh)
                            T.cancel wrTh
                        ) killThread
                fromClient <- C.bufferSource $ C.Source $ return $ connSource conn th
                fromClient C.$$ connSink uconn th
                return False
            Left errorMsg ->
                failRequest th conn req errorMsg ("PROXY FAILURE\r\n" `mappend` errorMsg)


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




