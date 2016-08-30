{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Servant.Subscriber.Client where


import           Control.Concurrent.Async
import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception             (SomeException, displayException)
import           Control.Exception.Lifted      (finally, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger          (MonadLogger, logDebug)
import           Control.Monad.Trans.Control   (MonadBaseControl)
import           Data.Aeson
import           Data.IORef                    (IORef, writeIORef)
import           Data.Map                      (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection as WS

import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response as Resp
import           Servant.Subscriber.Types    as S

type ClientMonitors = Map Path StatusMonitor

data Client api = Client {
    subscriber      :: !(Subscriber api)
  , monitors        :: !(TVar ClientMonitors)
  , readRequest     :: !(IO (Maybe Request))
  , writeResponse   :: !(Response -> IO ())
  , pongCommandRef  :: !(IORef (IO ()))
  , closeCommandRef :: !(TVar (IO ())) -- |< TVar so actions on it can happen in STM
  }

data StatusMonitor = StatusMonitor {
  monitorPath :: !Path
, requests    :: !(Set HttpRequest)
, monitor     :: !(TVar (RefCounted ResourceStatus))
, oldStatus   :: !ResourceStatus
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

snapshotRequests :: Snapshot -> Set HttpRequest
snapshotRequests = requests . fullMonitor

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
        WS.Text bs  -> return $ decode bs
        WS.Binary _ -> error "Sorry - binary connections currently unsupported!"
  , writeResponse = sendDataMessage c . WS.Text . encode
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
    let sub  = subscriber c
    monitors' <- readTVar $ monitors c
    let alreadySubscribed = Map.member path monitors'
    let
        insertRequest :: StatusMonitor -> StatusMonitor
        insertRequest mon = mon { requests = Set.insert req (requests mon) }

    modifyTVar' (monitors c)
      =<< if alreadySubscribed
        then
            return $ Map.adjust insertRequest path
        else do
            tState <- subscribe path sub
            stateVal <- refValue <$> readTVar tState
            return $ Map.insert path $ StatusMonitor path (Set.singleton req) tState stateVal

-- | Remove a Request, also unsubscribes from subscriber and deletes our monitor
--   if it was the last Request for the given path.
removeRequest :: Client api -> HttpRequest -> STM ()
removeRequest c req = do
    let sub = subscriber c
    let path = httpPath req
    monitors' <- readTVar (monitors c)
    let
      deleteRequest :: StatusMonitor -> StatusMonitor
      deleteRequest mon = mon { requests = Set.delete req (requests mon) }
    forM_ ( Map.lookup path monitors' ) $ \mon -> do
        let newMon = deleteRequest mon
        modifyTVar' (monitors c)
          =<< if Set.null (requests newMon)
              then do
                unsubscribeMonitor sub mon
                return $ Map.delete path
              else
                return $ Map.adjust deleteRequest path

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
      Just (Subscribe httpReq) -> handleSubscribe b c httpReq
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

handleSubscribe :: Backend backend => backend -> Client api -> HttpRequest -> IO ()
handleSubscribe b c req = getServerResponse b req $ \ response -> do
  mapM_ (writeResponse c) =<< case response of
    (Resp.Modified _ _) -> do
      atomically $ addRequest req c
      return [ Resp.Subscribed req, response ]
    _ -> return [ response ]

handleUnsubscribe :: Client api -> HttpRequest -> IO ()
handleUnsubscribe c req = do
  atomically $ removeRequest c req
  writeResponse c $ Unsubscribed req


runMonitor :: Backend backend => backend -> Client api -> IO ()
runMonitor b c = forever $ do
    changes <- atomically $ monitorChanges c
    mapM_ (handleUpdates b c) changes

handleUpdates :: Backend backend => backend -> Client api -> (Set HttpRequest, ResourceStatus) -> IO ()
handleUpdates b c (reqs, event) = case event of
    S.Deleted        -> writeResponse c $ Resp.Deleted (httpPath . Set.elemAt 0 $ reqs) -- |< elemAt is partial - but we should never have an empty set, so this should be fine! - Check, check double check - test suite?! Or change parameters to this function.
    ( S.Modified _ ) -> mapM_ (handleModified b c) reqs


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
    let isGoodStatus = status >= 200 && status < 300 -- We only accept success
    sendResponse $ if isGoodStatus
                   then
                     Resp.Modified req response
                   else
                     Resp.HttpRequestFailed req httpResponse
    return ResponseReceived

monitorChanges :: Client api -> STM [(Set HttpRequest, ResourceStatus)]
monitorChanges c = do
  snapshots <- mapM toSnapshot . Map.elems =<< readTVar (monitors c)
  let result = getChanges snapshots
  if null result
    then retry
    else do
      let newMonitors = monitorsFromList . updateMonitors $ snapshots
      writeTVar (monitors c) newMonitors
      return result


getChanges :: [Snapshot] -> [(Set HttpRequest, ResourceStatus)]
getChanges = map toChangeReport . filter monitorChanged

monitorChanged :: Snapshot -> Bool
monitorChanged m = snapshotCurrent m /= snapshotOld m

toChangeReport :: Snapshot -> (Set HttpRequest, ResourceStatus)
toChangeReport m = (snapshotRequests m, snapshotCurrent m)

updateMonitors :: [Snapshot] -> [StatusMonitor]
updateMonitors = map updateOldStatus . filter ((/= S.Deleted) . snapshotCurrent)

updateOldStatus :: Snapshot -> StatusMonitor
updateOldStatus m = (fullMonitor m) {
    oldStatus = snapshotCurrent m
  }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    paths = map (monitorPath) ms
    assList = zip paths ms
  in
    Map.fromList assList
