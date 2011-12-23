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
--   This code originated in Michael Snoyman's Warp package, was modified by
--   Stephen Blackheath to turn it into a HTTP/HTTP proxy and is now maintained
--   by Erik de Castro Lopo.
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

import Network    ( PortID(..) )
import Network.Socket
    ( accept, Family (..)
    , SocketType (Stream), listen, bindSocket, setSocketOption, maxListenQueue
    , SockAddr, SocketOption (ReuseAddr)
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    , Socket, sClose, shutdown, ShutdownCmd(..), HostName, ServiceName, socket, connect
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
import Control.Concurrent (forkIO, ThreadId, killThread)

import Data.Maybe (fromMaybe, isNothing)
import qualified Network.HTTP.Enumerator as HE

import Data.Typeable (Typeable)

import Data.Enumerator hiding (filter, head, foldl', map)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import Blaze.ByteString.Builder
    (copyByteString, Builder, toByteString, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Proxy.Timeout as T
import Data.List (delete, foldl')
import Control.Monad (forever, when)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPutStrLn, stderr)
import Numeric (readDec)
import Data.Int (Int64)

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

bindPort :: Int         -- ^ Port
         -> String      -- ^ Bind interface or "*" for all
         -> IO Socket
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
        sCloseX
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
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sCloseX s >> return Nothing
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
        sCloseX
        (runSettingsSocket set)
#endif

type Port = Int


runSettingsSocket :: Settings -> Socket -> IO ()
runSettingsSocket set sock = do
    let onE = proxyOnException set
        reqRM = proxyRequestModifier set
        port = proxyPort set
    tm <- T.initialize $ proxyTimeout set * 1000000
    mgr <- HE.newManager
    forever $ do
        (conn, sa) <- accept sock
        _ <- forkIO $ do
            th <- T.registerKillThread tm
            serveConnection th tm onE reqRM port conn sa mgr
            T.cancel th
        return ()

verboseSockets :: Bool
verboseSockets = False

sCloseX :: Socket -> IO ()
sCloseX s = do
    when verboseSockets $ putStrLn ("close " ++ show s)
    sClose s

shutdownX :: Socket -> ShutdownCmd -> IO ()
shutdownX s ShutdownReceive = do
    when verboseSockets $ putStrLn ("shutdown " ++ show s ++ " ShutdownReceive")
    shutdown s ShutdownReceive

shutdownX s ShutdownSend = do
    when verboseSockets $ putStrLn ("shutdown " ++ show s ++ " ShutdownSend")
    shutdown s ShutdownSend

shutdownX s ShutdownBoth = do
    when verboseSockets $ putStrLn ("shutdown " ++ show s ++ " ShutdownBoth")
    shutdown s ShutdownBoth

mkHeaders :: Monad m
          => H.HttpVersion
          -> H.Status
          -> H.ResponseHeaders
          -> E.Enumerator ByteString m b
mkHeaders ver s hrs =
    E.enumList 1 [toByteString $ headers ver s hrs False]


serveConnection :: T.Handle
                -> T.Manager
                -> (SomeException -> IO ())
                -> (Request -> IO Request)
                -> Port -> Socket -> SockAddr
                -> HE.Manager
                -> IO ()
serveConnection th tm onException requestMod port conn remoteHost' mgr = do
      mExtraSocket <- E.run_ (fromClient $$ serveConnection')
          `finally` sCloseX conn
      case mExtraSocket of
          Just (tid, s) -> do
              killThread tid
              sCloseX s
          Nothing -> return ()
    `catch` onException
  where
    fromClient = enumSocket th bytesPerRead conn

    serveConnection' :: E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
    serveConnection' = do
        req <- parseRequest port remoteHost'
        --liftIO $ print $ requestHeaders req
        case req of
            _ | requestMethod req `elem` [ "GET", "POST" ] ->
                liftIO (requestMod req) >>= proxyPlain
            _ | requestMethod req == "CONNECT" ->
                case B.split ':' (rawPathInfo req) of
                    [h, p] -> proxyConnect th tm onException conn h (readDecimal $ B.unpack p) req
                    _      -> failRequest th conn req "Bad request" ("Bad request '" `mappend` rawPathInfo req `mappend` "'.")
            _ | otherwise ->
                failRequest th conn req "Unknown request" ("Unknown request '" `mappend` rawPathInfo req `mappend` "'.")

    proxyPlain :: Request -> E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
    proxyPlain req = do
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
             else readDecimal . B.unpack . fromMaybe "0" . lookup "content-length" . requestHeaders $ req

        url <-
            (\u -> u { HE.method = requestMethod req,
                       HE.requestHeaders = outHdrs,
                       HE.rawBody = True,
                       HE.requestBody = HE.RequestBodyEnum contentLength
                                            $ joinE (enumIteratee contentLength lazyTakeMax)
                                            $ EL.map fromByteString
                                            })
                <$> lift (HE.parseUrl (B.unpack urlStr))

        close' <- E.run_ $ HE.http url (handleHttpReply close) mgr
        if close'
            then return Nothing
            else serveConnection'
      where
        handleHttpReply close status hdrs = do
            let remoteClose = isNothing ("content-length" `lookup` hdrs)
                close' = close || remoteClose
                hdrs' = [(n, v) | (n, v) <- hdrs, n `notElem`
                             ["connection", "proxy-connection"]
                        ]
                          ++ [("Connection", if close' then "Close" else "Keep-Alive")]
            mkHeaders (httpVersion req) status hdrs' $$ iterSocket th conn close
            return remoteClose

-- Create an Enumerator from an Iteratee.
-- The first parameter is the number of bytes to be pulled from the Iteratee.
-- The second is an Iteratee that can pull the data from the source in chunks.
-- The return value is an Enumerator that operates inside the Iteratee monad.
enumIteratee :: MonadIO m => Int64
             -> (Int -> Iteratee ByteString m ByteString)
             -> Enumerator ByteString (Iteratee ByteString m) c
enumIteratee maxlen takeMax = inner 0
  where
    blockLen = 32768
    inner count (Continue k)
        | count >= maxlen = k (Chunks [])
        | count + blockLen <= maxlen = do
                  bs <- lift $ takeMax $ fromIntegral blockLen
                  if B.null bs
                      then k EOF
                      else k (Chunks [bs]) >>== inner (count + blength bs)
        | otherwise = do
                  bs <- lift $ takeMax $ fromIntegral (maxlen - count)
                  if B.null bs
                      then k EOF
                      else k (Chunks [bs]) >>== inner (count + blength bs)
    inner _ step = returnI step
    blength = fromIntegral . B.length

-- Take up to maxlen bytes of data as lazily as possible. Avoid splitting and
-- joining ByteStrings as much as possible.
lazyTakeMax :: Monad m => Int -> Iteratee ByteString m ByteString
lazyTakeMax maxlen | maxlen <= 0 = return B.empty
lazyTakeMax maxlen = continue step
  where
    step (Chunks []) = continue step

    step (Chunks (x:xs))
        | B.length x < maxlen = yield x (Chunks xs)
        | otherwise =
                let (start, extra) = B.splitAt maxlen x
                in yield start (Chunks (extra:xs))

    step EOF = yield B.empty EOF

--------------------------------------------------------------------------------

failRequest :: T.Handle -> Socket -> Request -> ByteString -> ByteString -> E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
failRequest th conn req headerMsg bodyMsg = do
        EB.isolate 0 =$
            E.enumList 1 [bodyMsg] $$
            mkHeaders (httpVersion req) status [("Content-Length", B.pack . show . B.length $ bodyMsg)] $$
            iterSocket th conn True
        return Nothing
      where
        status = H.status500 { H.statusMessage = headerMsg }

proxyConnect :: T.Handle -> T.Manager -> (SomeException -> IO ()) -> Socket -> ByteString -> Int -> Request -> E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
proxyConnect th tm onException conn host prt req = do
        liftIO $ putStrLn $ B.unpack (requestMethod req) ++ " " ++ B.unpack host ++ ":" ++ show prt
        mHandles <- liftIO $ do
            s <- connectTo (B.unpack host) (PortNumber . fromIntegral $ prt)
            let eh = enumSocket th 65536 s
                ih = iterSocket th s True
            return $ Right (s, eh, ih)
          `catch` \(exc :: IOException) ->
            return $ Left $ "Unable to connect: " `mappend` B.pack (show exc)
        case mHandles of
            Right (s, eh, ih) -> do
                tid <- liftIO $ forkIO $ do
                    wrTh <- T.registerKillThread tm
                    E.run_ (eh $$ mkHeaders (httpVersion req) H.statusOK [] $$ iterSocket wrTh conn True)
                        `catch` onException
                    T.cancel wrTh
                ih
                return (Just (tid, s))
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

parseRequest :: Port -> SockAddr -> E.Iteratee S.ByteString IO Request
parseRequest port remoteHost' = do
    headers' <- takeHeaders
    parseRequest' port headers' remoteHost'

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

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> SockAddr
              -> E.Iteratee S.ByteString IO Request
parseRequest' _ [] _ = E.throwError $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath) =
            if S.null rpath'
                then ("","/")
                else if "http://" `S.isPrefixOf` rpath'
                         then S.breakByte 47 $ S.drop 7 rpath' -- '/'
                         else ("", rpath')
    let heads = map parseHeaderNoAttr otherLines
    let host = case (host', lookup "host" heads) of
            ("", Just h) -> h
            (h, _)       -> h
    return Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = H.decodePathSegments rpath
                , rawPathInfo = rpath
                , rawQueryString = gets
                , queryString = H.parseQuery gets
                -- NOTE! The meaning of serverName is different from what it is in standard
                -- WAI, in the following way:
                -- 1. Here it includes the port number. Otherwise there is no way to obtain it.
                -- 2. Where 'Host:' is also supplied, we prefer the URI host, since it has the.
                --    (WAI gives preference to the 'Host:' header.)
                , serverName = host  -- serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , remoteHost = remoteHost'
                }

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, H.HttpVersion)
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
               else E.throwError NonHttp
        _ -> E.throwError $ BadFirstLine $ B.unpack s
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

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                   then SU.unsafeDrop 2 rest
                   else rest
     in (CI.mk k, rest')

enumSocket :: T.Handle -> Int -> Socket -> E.Enumerator ByteString IO a
enumSocket th len sock =
    inner
  where
    inner (E.Continue k) = do
        bs <- liftIO $ Sock.recv sock len
        liftIO $ T.tickle th
        if S.null bs
            then do
                liftIO $
                    shutdownX sock ShutdownReceive
                  `catch` \(exc :: IOException) ->
                    putStrLn $ "couldn't shutdown read side of " ++ show sock ++ ": " ++ show exc
                E.continue k
            else k (E.Chunks [bs]) >>== inner
    inner step = E.returnI step
------ The functions below are not warp-specific and could be split out into a
--separate package.

iterSocket :: MonadIO m
           => T.Handle
           -> Socket
           -> Bool  -- ^ To close
           -> E.Iteratee B.ByteString m ()
iterSocket th sock toClose =
    E.continue step
  where
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when the proxy is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.
    step E.EOF = do
        liftIO $ T.resume th
        when toClose $
            liftIO $
                shutdownX sock ShutdownSend
              `catch` \(exc :: IOException) ->
                putStrLn $ "couldn't shutdown send side of " ++ show sock ++ ": " ++ show exc
        E.yield () E.EOF
    step (E.Chunks []) = E.continue step
    step (E.Chunks xs) = do
        liftIO $ T.resume th
        liftIO $ Sock.sendMany sock xs
        liftIO $ T.pause th
        E.continue step

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

takeHeaders :: E.Iteratee ByteString IO [ByteString]
takeHeaders = do
  !x <- forceHead ConnectionClosedByPeer
  takeHeaders' 0 id id x

{-# INLINE takeHeaders #-}

takeHeaders' :: Int
             -> ([ByteString] -> [ByteString])
             -> ([ByteString] -> [ByteString])
             -> ByteString
             -> E.Iteratee S.ByteString IO [ByteString]
takeHeaders' !len _ _ _ | len > maxTotalHeaderLength = E.throwError OverLargeHeader
takeHeaders' !len !lines !prepend !bs = do
  let !bsLen = {-# SCC "takeHeaders'.bsLen" #-} S.length bs
      !mnl = {-# SCC "takeHeaders'.mnl" #-} S.elemIndex 10 bs
  case mnl of
       -- no newline.  prepend entire bs to next line
       !Nothing -> {-# SCC "takeHeaders'.noNewline" #-} do
         let !len' = len + bsLen
         !more <- forceHead IncompleteHeaders
         takeHeaders' len' lines (prepend . (:) bs) more
       Just !nl -> {-# SCC "takeHeaders'.newline" #-} do
         let !end = nl
             !start = nl + 1
             !line = {-# SCC "takeHeaders'.line" #-}
                     if end > 0
                        -- line data included in this chunk
                        then S.concat $! prepend [SU.unsafeTake (checkCR bs end) bs]
                        --then S.concat $! prepend [SU.unsafeTake (end-1) bs]
                        -- no line data in this chunk (all in prepend, or empty line)
                        else S.concat $! prepend []
         if S.null line
            -- no more headers
            then {-# SCC "takeHeaders'.noMoreHeaders" #-} do
              let !lines' = {-# SCC "takeHeaders'.noMoreHeaders.lines'" #-} lines []
              if start < bsLen
                 then {-# SCC "takeHeaders'.noMoreHeaders.yield" #-} do
                   let !rest = {-# SCC "takeHeaders'.noMoreHeaders.yield.rest" #-} SU.unsafeDrop start bs
                   E.yield lines' $! E.Chunks [rest]
                 else return lines'

            -- more headers
            else {-# SCC "takeHeaders'.moreHeaders" #-} do
              let !len' = len + start
                  !lines' = {-# SCC "takeHeaders.lines'" #-} lines . (:) line
              !more <- {-# SCC "takeHeaders'.more" #-}
                       if start < bsLen
                          then return $! SU.unsafeDrop start bs
                          else forceHead IncompleteHeaders
              {-# SCC "takeHeaders'.takeMore" #-} takeHeaders' len' lines' id more
{-# INLINE takeHeaders' #-}

forceHead :: InvalidRequest -> E.Iteratee ByteString IO ByteString
forceHead err = do
  !mx <- EL.head
  case mx of
       !Nothing -> E.throwError err
       Just !x -> return x
{-# INLINE forceHead #-}

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


readDecimal :: Num a => String -> a
readDecimal s =
    case readDec s of
      [] -> 0
      (x, _):_ -> x

