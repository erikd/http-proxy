{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
--
-- HTTP Proxy with a lot of code taken from Michael Snoyman's Warp, modified by
-- Stephen Blackheath.
--
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Dependencies:
--   case-insensitive
--   http-enumerator
--   unix-compat
--   wai
--
---------------------------------------------------------

module Main where

import Prelude hiding (catch, lines)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( PortID(..) )
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
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException, AsyncException (ThreadKilled)
    , bracketOnError, IOException, throw
    )
import Control.Concurrent (forkIO, ThreadId, killThread)
import qualified Data.Char as C
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Data.Enumerator (($$), (=$), (>>==))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mconcat)

import qualified System.PosixCompat.Files as P

import Control.Monad.IO.Class (liftIO)
import qualified Timeout as T
import Timeout (Manager, registerKillThread, pause, resume)
import Data.Word (Word8)
import Data.List (foldl')
import Control.Monad (forever, when)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPutStrLn, stderr, hSetBuffering, BufferMode(NoBuffering))
import Data.Version (showVersion)

import Data.List (delete)

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
        (sCloseX)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )

main = run 31081

-- | Run an 'Application' on the given port. This calls 'runSettings' with
-- 'defaultSettings'.
run :: Port -> IO ()
run p = runSettings defaultSettings { settingsPort = p }

-- | Run a Warp server with the given settings.
runSettings :: Settings -> IO ()
#if WINDOWS
runSettings set = withSocketsDo $ do
    var <- MV.newMVar Nothing
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sCloseX s >> return Nothing
    _ <- forkIO $ bracket
        (bindPort (settingsPort set) (settingsHost set))
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            runSettingsSocket set s)
    forever (threadDelay maxBound) `finally` clean
#else
runSettings set =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sCloseX
        (runSettingsSocket set)
#endif

type Port = Int

-- | Same as 'runSettings', but uses a user-supplied socket instead of opening
-- one. This allows the user to provide, for example, Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSettingsSocket :: Settings -> Socket -> IO ()
runSettingsSocket set socket = do
    let onE = settingsOnException set
        port = settingsPort set
    tm <- T.initialize $ settingsTimeout set * 1000000
    forever $ do
        (conn, sa) <- accept socket
        _ <- forkIO $ do
            th <- T.register tm (return()) -- T.registerKillThread tm
            serveConnection th tm onE port conn sa
            T.cancel th
        return ()

verboseSockets = False
sCloseX s = do
    when verboseSockets $ putStrLn ("close "++show s)
    sClose s
shutdownX s ShutdownReceive = do
    when verboseSockets $ putStrLn ("shutdown "++show s++" ShutdownReceive")
    shutdown s ShutdownReceive
shutdownX s ShutdownSend = do
    when verboseSockets $ putStrLn ("shutdown "++show s++" ShutdownSend")
    shutdown s ShutdownSend

serveConnection :: T.Handle
                -> T.Manager
                -> (SomeException -> IO ())
                -> Port -> Socket -> SockAddr -> IO ()
serveConnection th tm onException port conn remoteHost' = do
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
    mkHeaders req s hrs = E.enumList 1 [toByteString $ headers (httpVersion req) s hrs False]

    serveConnection' :: E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
    serveConnection' = do
        req <- parseRequest port remoteHost'
        liftIO $ print $ requestHeaders req
        case req of
            _ | requestMethod req == "CONNECT" ->
                case B.split ':' (rawPathInfo req) of
                    [h, p] -> proxy h (read $ B.unpack p) req
                    _	-> failRequest req "Bad request" ("Bad request '" `mappend` rawPathInfo req `mappend` "'.")
            _ | otherwise ->
                failRequest req "Unknown request" ("Unknown request '" `mappend` rawPathInfo req `mappend` "'.")

    failRequest :: Request -> ByteString -> ByteString -> E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
    failRequest req headerMsg bodyMsg = do
        EB.isolate 0 =$
            E.enumList 1 [bodyMsg] $$
            mkHeaders req status [("Content-Length", B.pack . show . B.length $ bodyMsg)] $$
            iterSocket th conn
        return Nothing
      where
        status = H.status500 {
                H.statusMessage = headerMsg
            }

    proxy :: ByteString -> Int -> Request -> E.Iteratee ByteString IO (Maybe (ThreadId, Socket))
    proxy host port req = do
        mHandles <- liftIO $ do
            s <- connectTo (B.unpack host) (PortNumber . fromIntegral $ port)
            let eh = enumSocket th 65536 s
                ih = iterSocket th s
            return $ Right (s, eh, ih)
          `catch` \(exc :: IOException) -> do
            return $ Left $ "Unable to connect: " `mappend` B.pack (show exc)
        case mHandles of
            Right (s, eh, ih) -> do
                tid <- liftIO $ forkIO $ do
                    wrTh <- T.register tm (return()) -- T.registerKillThread tm
                    (E.run_ $ eh $$ mkHeaders req H.statusOK [] $$ iterSocket wrTh conn)
                        `catch` onException
                    T.cancel wrTh
                ih
                return (Just (tid, s))
            Left errorMsg ->
                failRequest req errorMsg ("PROXY FAILURE\r\n" `mappend` errorMsg)

connectTo :: HostName		-- Hostname
	  -> PortID 		-- Port Identifier
	  -> IO Socket		-- Connected Socket
connectTo hostname (Service serv) = connect' hostname serv
connectTo hostname (PortNumber port) = connect' hostname (show port)

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
	(sClose)  -- only done if there's an error
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
    let host = fromMaybe host' $ lookup "host" heads
    let serverName' = takeUntil 58 host -- ':'
    return $ Request
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
                }

takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

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
                `mappend` (copyByteString $
                            case httpversion of
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
hasBody s req = s /= (H.Status 204 "") && s /= H.status304 &&
                H.statusCode s >= 200 && requestMethod req /= "HEAD"

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
enumSocket th len socket =
    inner
  where
    inner (E.Continue k) = do
        bs <- liftIO $ Sock.recv socket len
        liftIO $ T.tickle th
        if S.null bs
            then do
                liftIO $ do
                    shutdownX socket ShutdownReceive
                  `catch` \(exc :: IOException) ->
                    putStrLn $ "couldn't shutdown read side of "++show socket++": "++show exc
                E.continue k
            else k (E.Chunks [bs]) >>== inner
    inner step = E.returnI step
------ The functions below are not warp-specific and could be split out into a
--separate package.

iterSocket :: T.Handle
           -> Socket
           -> E.Iteratee B.ByteString IO ()
iterSocket th sock =
    E.continue step
  where
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when Warp is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.
    step E.EOF = do
        liftIO $ T.resume th
        liftIO $ do
            shutdownX sock ShutdownSend
          `catch` \(exc :: IOException) ->
            putStrLn $ "couldn't shutdown send side of "++show sock++": "++show exc
        E.yield () E.EOF
    step (E.Chunks []) = E.continue step
    step (E.Chunks xs) = do
        liftIO $ T.resume th
        liftIO $ Sock.sendMany sock xs
        liftIO $ T.pause th
        E.continue step

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { settingsTimeout = 20 }
data Settings = Settings
    { settingsPort :: Int -- ^ Port to listen on. Default value: 3000
    , settingsHost :: String -- ^ Host to bind to, or * for all. Default value: *
    , settingsOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = "*"
    , settingsOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing ->
                if go' $ fromException e
                    then hPutStrLn stderr $ show e
                    else return ()
    , settingsTimeout = 30
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

-- Note: This function produces garbage on invalid input. But serving an
-- invalid content-length is a bad idea, mkay?
readInt :: S.ByteString -> Integer
readInt = S.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0

-- | Call the inner function with a timeout manager.
withManager :: Int -- ^ timeout in microseconds
            -> (Manager -> IO a)
            -> IO a
withManager timeout f = do
    -- FIXME when stopManager is available, use it
    man <- T.initialize timeout
    f man

serverHeader :: H.RequestHeaders -> H.RequestHeaders
serverHeader hdrs = case lookup key hdrs of
    Nothing  -> server : hdrs
    Just svr -> servers svr : delete (key,svr) hdrs
 where
    key = "Server"
    ver = B.pack $ "Proxy/0.0"
    server = (key, ver)
    servers svr = (key, S.concat [svr, " ", ver])
