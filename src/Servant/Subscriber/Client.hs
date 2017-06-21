{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Subscriber.Client where


import           Control.Concurrent.Async
import           Control.Lens
import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception             (SomeException, displayException)
import           Control.Exception.Lifted      (finally, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger          (MonadLogger, logDebug)
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import           Data.IORef                    (IORef, writeIORef)
import           Data.Map                      (Map)
import qualified Data.Map.Strict               as Map
import           Data.Monoid                   ((<>))
import           Data.Foldable                 (traverse_)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection as WS
import Debug.Trace (trace)

import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response as Resp
import           Servant.Subscriber.Types    as S

type ClientMonitors = Map HttpRequest StatusMonitor

data Client api = Client {
    subscriber      :: !(Subscriber api)
  , monitors        :: !(TVar ClientMonitors)
  , readRequest     :: !(IO (Maybe Request))
  , writeResponse   :: !(Response -> IO ())
  , pongCommandRef  :: !(IORef (IO ()))
  , closeCommandRef :: !(TVar (IO ())) -- TVar so actions on it can happen in STM
  }

data StatusMonitor = StatusMonitor {
  request     :: !HttpRequest
, monitor     :: !(TVar (RefCounted ResourceStatus))
, oldStatus   :: !(Maybe ResourceStatus) -- Nothing when added so we get a notification in any case.
}

data Snapshot = Snapshot {
  snapshotCurrent :: ResourceStatus
, fullMonitor     :: StatusMonitor
}

snapshotOld :: Snapshot -> Maybe ResourceStatus
snapshotOld = oldStatus . fullMonitor

monitorPath :: StatusMonitor -> Path
monitorPath = httpPath . request

toSnapshot :: StatusMonitor -> STM Snapshot
toSnapshot mon = do
  current <- readTVar $ monitor mon
  return Snapshot {
    snapshotCurrent = refValue current
  , fullMonitor = mon
  }

snapshotRequest :: Snapshot -> HttpRequest
snapshotRequest = request . fullMonitor

fromWebSocket :: Subscriber api -> IORef (IO ()) -> WS.Connection -> STM (Client api)
fromWebSocket sub myRef c = do
  ms <- newTVar Map.empty
  closeVar <- newTVar (return ())
  return Client {
    subscriber = sub
  , monitors = ms
  , readRequest = do
      msg <- WS.receiveDataMessage c
      case msg of
#ifdef WEBSOCKETS_0_11
        WS.Text bs _ -> return $ decode bs
#else
        WS.Text bs  -> return $ decode bs
#endif
        WS.Binary _ -> error "Sorry - binary connections currently unsupported!"
#ifdef WEBSOCKETS_0_11
  , writeResponse = \msg -> sendDataMessage c $ WS.Text (encode msg) Nothing
#else
  , writeResponse = sendDataMessage c . WS.Text . encode
#endif
  , pongCommandRef = myRef
  , closeCommandRef = closeVar
  }

run :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, Backend backend)
       => backend -> Client api -> m ()
run b c = do
  let
    sub     = subscriber c
    work    = liftIO $ race_ (runMonitor b c) (handleRequests b c)
    cleanup = do
      close <- liftIO . atomically $ do
        ms <- readTVar (monitors c)
        mapM_ (unsubscribeMonitor sub) ms
        readTVar (closeCommandRef c)
      liftIO close
  r <- try $ finally work cleanup
  case r of
    Left e -> $logDebug $ T.pack $ displayException (e :: SomeException)
    Right _ -> return ()

addRequest :: HttpRequest -> Client api -> STM ()
addRequest req c = do
    let path = httpPath req
    trace ("Added request for PATH:" <> show path) $ pure ()
    let sub  = subscriber c
    tState <- subscribe path sub
    modifyTVar' (monitors c) $ at req .~ Just (StatusMonitor req tState Nothing)

-- | Remove a Request, also unsubscribes from subscriber and deletes our monitor
--   if it was the last Request for the given path.
removeRequest :: Client api -> HttpRequest -> STM ()
removeRequest c req = do
    let sub = subscriber c
    monitors' <- readTVar (monitors c)
    traverse_ (unsubscribeMonitor sub) $ monitors'^.at req
    modifyTVar' (monitors c) $ at req .~ Nothing

-- | Does not remove the monitor - use removeRequest if you want this!
unsubscribeMonitor :: Subscriber api -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    path = monitorPath m
    mon = monitor m
  in
    unsubscribe path mon sub


handleRequests :: Backend backend => backend -> Client api -> IO ()
handleRequests b c = forever $ do
    req <- readRequest c
    case req of
      Nothing                  -> writeResponse c ParseError
      Just (Subscribe httpReq) -> handleSubscribe c httpReq
      Just (Unsubscribe path)  -> handleUnsubscribe c path
      Just (SetPongRequest httpReq) -> do
        let doIt = doRequestIgnoreResult httpReq
        writeIORef (pongCommandRef c) doIt
        doIt
        writeResponse c $ Subscribed httpReq
      Just (SetCloseRequest httpReq) -> do
        let doIt = doRequestIgnoreResult httpReq
        atomically $ writeTVar (closeCommandRef c) doIt
        writeResponse c $ Subscribed httpReq
 where
   doRequestIgnoreResult :: HttpRequest -> IO ()
   doRequestIgnoreResult req' = void $ requestResource b req' (const (pure ResponseReceived))

handleSubscribe :: Client api -> HttpRequest -> IO ()
handleSubscribe c req = do
  atomically $ addRequest req c
  writeResponse c $ Resp.Subscribed req

handleUnsubscribe :: Client api -> HttpRequest -> IO ()
handleUnsubscribe c req = do
  atomically $ removeRequest c req
  writeResponse c $ Unsubscribed req


runMonitor :: Backend backend => backend -> Client api -> IO ()
runMonitor b c = forever $ do
    changes <- atomically $ monitorChanges c
    mapM_ (handleUpdates b c) changes

handleUpdates :: Backend backend => backend -> Client api -> (HttpRequest, ResourceStatus) -> IO ()
handleUpdates b c (req, event) = case event of
    S.Deleted        -> writeResponse c $ Resp.Deleted (httpPath req)
    ( S.Modified _ ) -> handleModified b c req


handleModified :: Backend backend => backend -> Client api -> HttpRequest -> IO ()
handleModified b c req = getServerResponse b req handleResponse
  where
    handleResponse :: Response -> IO ()
    handleResponse resp = do
      case resp of
        HttpRequestFailed _ _ -> atomically $ removeRequest c req
        _                     -> return ()
      writeResponse c resp

getServerResponse :: Backend backend => backend -> HttpRequest -> (Response -> IO ()) -> IO ()
getServerResponse b req sendResponse = void $ requestResource b req
  $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let response = Resp.httpBody httpResponse
    T.putStrLn $ "Got server response body: " <> T.decodeUtf8 (BS.toStrict . encode $ response)
    T.putStrLn $ "For request: " <> T.pack (show req)
    let isGoodStatus = status >= 200 && status < 300 -- We only accept success
    sendResponse $ if isGoodStatus
                   then
                     Resp.Modified req response
                   else
                     Resp.HttpRequestFailed req httpResponse
    return ResponseReceived

monitorChanges :: Client api -> STM [(HttpRequest, ResourceStatus)]
monitorChanges c = do
  snapshots <- mapM toSnapshot . Map.elems =<< readTVar (monitors c)
  let result = getChanges snapshots
  if null result
    then retry
    else do
      let newMonitors = monitorsFromList . updateMonitors $ snapshots
      writeTVar (monitors c) newMonitors
      return result


getChanges :: [Snapshot] -> [(HttpRequest, ResourceStatus)]
getChanges = map toChangeReport . filter monitorChanged

monitorChanged :: Snapshot -> Bool
monitorChanged m = Just (snapshotCurrent m) /= snapshotOld m

toChangeReport :: Snapshot -> (HttpRequest, ResourceStatus)
toChangeReport m = (snapshotRequest m, snapshotCurrent m)

updateMonitors :: [Snapshot] -> [StatusMonitor]
updateMonitors = map updateOldStatus . filter ((/= S.Deleted) . snapshotCurrent)

updateOldStatus :: Snapshot -> StatusMonitor
updateOldStatus m = (fullMonitor m) {
    oldStatus = Just $ snapshotCurrent m
  }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    reqs = map (request) ms
    assList = zip reqs ms
  in
    Map.fromList assList
