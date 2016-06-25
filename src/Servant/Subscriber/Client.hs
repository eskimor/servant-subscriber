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
import           Control.Monad.Logger (MonadLogger, logDebug, logError, logInfo, monadLoggerLog)
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

data Client = Client {
    monitors      :: !(TVar ClientMonitors)
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

fromWebSocket :: WS.Connection -> STM Client
fromWebSocket c = do
  ms <- newTVar Map.empty
  return Client {
    monitors = ms
  , readRequest = do
      msg <- WS.receiveDataMessage c
      case msg of
        WS.Text bs  -> return $ decode bs
        WS.Binary _ -> error "Sorry - binary connections currently unsupported!"
  , writeResponse = sendDataMessage c . WS.Text . encode
  }

run :: (MonadLogger m, MonadBaseControl IO m, MonadIO m, Backend backend) => backend -> Subscriber api -> Client -> m ()
run b sub c = do
  let
    work    = liftIO $ race_ (runMonitor b c) (handleRequests b sub c)
    cleanup = liftIO . atomically $ do
      ms <- readTVar (monitors c)
      mapM_ (unsubscribeMonitor sub) ms
  r <- try $ finally work cleanup
  case r of
    Left e -> $logDebug $ T.pack $ displayException (e :: SomeException)
    Right _ -> return ()

unsubscribeMonitor :: Subscriber api -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    path = httpPath . request $ m
    mon = monitor m
  in
    unsubscribe path mon sub

subscribeMonitor :: Subscriber api -> HttpRequest -> Client -> STM ()
subscribeMonitor sub req c = do
  let path = httpPath req
  tState <- subscribe path sub
  stateVal <- refValue <$> readTVar tState
  modifyTVar' (monitors c) $ Map.insert path (StatusMonitor req tState stateVal)

handleRequests :: Backend backend => backend -> Subscriber api -> Client -> IO ()
handleRequests b sub c = forever $ do
    req <- readRequest c
    case req of
      Nothing                 -> writeResponse c (RequestError ParseError)
      Just (Subscribe req)    -> handleSubscribe b sub c req
      Just (Unsubscribe path) -> handleUnsubscribe b sub c path

handleSubscribe :: Backend backend => backend -> Subscriber api -> Client -> HttpRequest -> IO ()
handleSubscribe b sub c req = void $ requestResource b req $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let path = httpPath req
    let isGoodStatus = status >= 200 && status < 300 -- For now we only accept success
    if isGoodStatus
      then
        mapM_ (writeResponse c) <=< atomically $ do
          ms <- readTVar (monitors c)
          case Map.lookup path ms of
            Just _  -> return [RequestError (AlreadySubscribed path)]
            Nothing -> do
              subscribeMonitor sub req c
              return [Resp.Subscribed path, Resp.Modified path httpResponse]
      else
        writeResponse c $ RequestError (HttpRequestFailed req httpResponse)
    return ResponseReceived

handleUnsubscribe :: Backend backend => backend -> Subscriber api -> Client -> Path -> IO ()
handleUnsubscribe b sub c path = writeResponse c <=< atomically $ do
    ms <- readTVar (monitors c)
    case Map.lookup path ms of
      Nothing -> return $ RequestError (NoSuchSubscription path)
      Just m -> do
        unsubscribeMonitor sub m
        modifyTVar (monitors c) $ Map.delete path
        return $ Unsubscribed path

runMonitor :: Backend backend => backend -> Client -> IO ()
runMonitor b c = forever $ do
    changes <- atomically $ monitorChanges c
    mapM_ (sendUpdate b (writeResponse c)) changes

sendUpdate :: Backend backend => backend -> (Response -> IO ()) -> (HttpRequest, ResourceStatus) -> IO ()
sendUpdate b sendResponse (req, S.Deleted)    = sendResponse $ Resp.Deleted (httpPath req)
sendUpdate b sendResponse (req, S.Modified _) = sendServerResponse b req sendResponse

sendServerResponse :: Backend backend => backend -> HttpRequest -> (Response -> IO ()) -> IO ()
sendServerResponse b req sendResponse = void $ requestResource b req
  $ \ httpResponse -> do
    let path = httpPath req
    sendResponse $ Resp.Modified path httpResponse
    return ResponseReceived

monitorChanges :: Client -> STM [(HttpRequest, ResourceStatus)]
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
