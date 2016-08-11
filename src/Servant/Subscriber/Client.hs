{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Servant.Subscriber.Client where


import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception (displayException, SomeException)
import           Control.Exception.Lifted (finally, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger, logDebug)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import           Network.WebSockets.Connection as WS

import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response as Resp
import           Servant.Subscriber.Types as S

type ClientMonitors = Map Path StatusMonitor

data Client api = Client {
    subscriber    :: !(Subscriber api)
  , monitors      :: !(TVar ClientMonitors)
  , readRequest   :: !(IO (Maybe Request))
  , writeResponse :: !(Response -> IO ())
  }

data StatusMonitor = StatusMonitor {
  request   :: !HttpRequest
, monitor   :: !(TVar (RefCounted ResourceStatus))
, oldStatus :: !ResourceStatus
}

data Snapshot = Snapshot {
  snapshotCurrent :: ResourceStatus
, fullMonitor     :: StatusMonitor
}

snapshotOld :: Snapshot -> ResourceStatus
snapshotOld = oldStatus . fullMonitor

toSnapshot :: StatusMonitor -> STM Snapshot
toSnapshot mon = do
  current <- readTVar $ monitor mon
  return Snapshot {
    snapshotCurrent = refValue current
  , fullMonitor = mon
  }

snapshotRequest :: Snapshot -> HttpRequest
snapshotRequest = request . fullMonitor

fromWebSocket :: Subscriber api -> WS.Connection -> STM (Client api)
fromWebSocket sub c = do
  ms <- newTVar Map.empty
  return Client {
    subscriber = sub
  , monitors = ms
  , readRequest = do
      msg <- WS.receiveDataMessage c
      case msg of
        WS.Text bs  -> return $ decode bs
        WS.Binary _ -> error "Sorry - binary connections currently unsupported!"
  , writeResponse = sendDataMessage c . WS.Text . encode
  }

run :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, Backend backend)
       => backend -> Client api -> m ()
run b c = do
  let
    sub     = subscriber c
    work    = liftIO $ race_ (runMonitor b c) (handleRequests b c)
    cleanup = liftIO . atomically $ do
      ms <- readTVar (monitors c)
      mapM_ (unsubscribeMonitor sub) ms
  r <- try $ finally work cleanup
  case r of
    Left e -> $logDebug $ T.pack $ displayException (e :: SomeException)
    Right _ -> return ()

addMonitor :: HttpRequest -> Client api -> STM ()
addMonitor req c = do
  let path = httpPath req
  let sub  = subscriber c
  tState <- subscribe path sub
  stateVal <- refValue <$> readTVar tState
  modifyTVar' (monitors c) $ Map.insert path (StatusMonitor req tState stateVal)

-- | Unsubscribe from subscriber and also delete our monitor
removeMonitor :: Client api -> Path -> STM ()
removeMonitor c path = do
    let sub = subscriber c
    ms <- readTVar (monitors c)
    forM_ ( Map.lookup path ms ) $ \m -> do
        unsubscribeMonitor sub m
        modifyTVar (monitors c) $ Map.delete path

-- | Does not remove the monitor - use removeMonitor if you want this!
unsubscribeMonitor :: Subscriber api -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    path = httpPath . request $ m
    mon = monitor m
  in
    unsubscribe path mon sub


handleRequests :: Backend backend => backend -> Client api -> IO ()
handleRequests b c = forever $ do
    req <- readRequest c
    case req of
      Nothing                  -> writeResponse c ParseError
      Just (Subscribe httpReq) -> handleSubscribe b c httpReq
      Just (Unsubscribe path)  -> handleUnsubscribe c path

handleSubscribe :: Backend backend => backend -> Client api -> HttpRequest -> IO ()
handleSubscribe b c req = getServerResponse b req $ \ response -> do
  let path = httpPath req
  mapM_ (writeResponse c) =<< case response of
    (Resp.Modified _ _) -> do
      atomically $ addMonitor req c
      return [ Resp.Subscribed path, response ]
    _ -> return [ response ]

handleUnsubscribe :: Client api -> Path -> IO ()
handleUnsubscribe c path = do
  atomically $ removeMonitor c path
  writeResponse c $ Unsubscribed path


runMonitor :: Backend backend => backend -> Client api -> IO ()
runMonitor b c = forever $ do
    changes <- atomically $ monitorChanges c
    mapM_ (handleUpdate b c) changes

handleUpdate :: Backend backend => backend -> Client api -> (HttpRequest, ResourceStatus) -> IO ()
handleUpdate b c (req, event) = case event of
    S.Deleted        -> sendResponse $ Resp.Deleted (httpPath req)
    ( S.Modified _ ) -> getServerResponse b req handleResponse
  where
    sendResponse :: Response -> IO ()
    sendResponse = writeResponse c

    handleResponse :: Response -> IO ()
    handleResponse resp = case resp of
      HttpRequestFailed _ _ -> do
        atomically $ removeMonitor c (httpPath req)
        sendResponse resp
      _                     ->
        sendResponse resp

getServerResponse :: Backend backend => backend -> HttpRequest -> (Response -> IO ()) -> IO ()
getServerResponse b req sendResponse = void $ requestResource b req
  $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let response = Resp.httpBody httpResponse
    let path = httpPath req
    let isGoodStatus = status >= 200 && status < 300 -- We only accept success
    sendResponse $ if isGoodStatus
                   then
                     Resp.Modified path response
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
monitorChanged m = snapshotCurrent m /= snapshotOld m

toChangeReport :: Snapshot -> (HttpRequest, ResourceStatus)
toChangeReport m = (snapshotRequest m, snapshotCurrent m)

updateMonitors :: [Snapshot] -> [StatusMonitor]
updateMonitors = map updateOldStatus . filter ((/= S.Deleted) . snapshotCurrent)

updateOldStatus :: Snapshot -> StatusMonitor
updateOldStatus m = (fullMonitor m) {
    oldStatus = snapshotCurrent m
  }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    paths = map (httpPath . request) ms
    assList = zip paths ms
  in
    Map.fromList assList
