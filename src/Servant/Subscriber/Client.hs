{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Client where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.Async
import           Control.Concurrent.STM          (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad                   (void)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString                 as BS
import qualified Data.CaseInsensitive            as Case
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.Map                        (Map)
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as T
import           Data.Time
import           GHC.Generics
import qualified Network.HTTP.Types              as H
import           Network.WebSockets.Connection   as WS
import           Servant.Server

import           Servant.Subscriber
import           Servant.Subscriber.Types
import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response as Resp
import           Servant.Subscriber.Subscribable

type ClientMonitors = Map Path StatusMonitor

data Client = Client {
    monitors      :: !(TVar ClientMonitors)
  , readRequest   :: !(IO Request)
  , writeResponse :: !(Response -> IO ())
  }

data StatusMonitor = StatusMonitor {
  request   :: !Request
, monitor   :: !(TVar (RefCounted ResourceStatus))
, oldStatus :: !ResourceStatus
}

run :: Backend backend => backend -> Subscriber -> Client -> IO ()
run b sub c = do
  let
    work    = race (monitorChanges b c) (handleRequests sub c)
    cleanup = atomically $ do
      monitors <- readTVar (watches c)
      mapM_ (unsubscribeMonitor sub) monitors
  finally work cleanup

unsubscribeMonitor :: Subscriber -> Monitor -> STM ()
unsubscribeMonitor sub m =
  let
    path = requestPath . request $ m
    mon = monitor m
  in
    unsubscribe path mon sub

subscribeMonitor :: Subscriber -> Request -> Client -> STM ()
subscribeMonitor sub req c = do
  let path = requestPath req
  tState <- subscribe path sub
  stateVal <- refValue <$> readTVar tState
  modifyTVar' (monitors c) . Map.insert path (StatusMonitor req state stateVal)

handleRequests :: Backend backend => backend -> Subscriber -> Client -> IO ()
handleRequests b sub c = forever $ do
    req <- readRequest c
    case action of
      Subscribe req    -> handleSubscribe b sub c req
      Unsubscribe path -> handleUnsubscribe b sub c path

handleSubscribe :: Backend backend => backend -> Subscriber -> Client -> Request -> IO ()
handleSubscribe b sub c req = sendRequest b req $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let path = requestPath req
    let isGoodStatus = status >= 200 && status < 300 -- For now we only accept success
    if isGoodStatus
      then
        writeResponse c <<= atomically $
          case Map.lookup path (monitors c) of
            Just _  -> return $ RequestError (Subscribe Req) AlreadySubscribed
            Nothing -> do
              subscribeMonitor sub req c
              return $ Modified path httpResponse
      else
        writeResponse c $ RequestError (Subscribe req) (ServerError httpResponse)

handleUnsubscribe :: Backend backend => backend -> Subscriber -> Client -> Path -> IO ()
handleUnsubscribe b sub c path = writeResponse c <<= atomically $ do
    ms <- readTVar (monitors c)
    case Map.lookup path ms of
      Nothing -> return $ RequestError (Unsubscribe path) NoSuchSubscription
      Just m -> do
        unsubscribeMonitor s m
        writeTVar (monitors c) $ Map.delete path
        return $ Unsubscribed path

runMonitor :: Backend backend => backend -> Client -> IO ()
runMonitor b c = forever $ do
    changes <- atomically $ monitorChanges c
    mapM_ (sendUpdate b (writeResponse c)) changes

sendUpdate :: Backend backend => backend -> (Response -> IO ()) -> (Request, ResourceStatus) -> IO ()
sendUpdate b sendResponse (req, Deleted)    = sendResponse $ Deleted (requestPath req)
sendUpdate b sendResponse (req, Modified _) = sendServerResponse backend req sendResponse

sendServerResponse :: Backend backend => backend -> Request -> (Response -> IO ()) -> IO ()
sendServerResponse b req sendResponse = void $ sendRequest b (httpData req)
  $ \ httpResponse -> do
    let path = requestPath req
    sendResponse $ Modified path httpResponse
    return ResponseReceived

monitorChanges :: Client -> STM [(Request, ResourceStatus)]
monitorChanges c = do
      ms <- elems <$> readTVar (monitors c)
      result <- getChanges ms
      if null result
        then retry
        else do
          writeTVar (monitors c) . monitorsFromList . updateMonitors $ ms
          return result


getChanges :: [StatusMonitor] -> STM [(Request, ResourceStatus)]
getChanges = mapM toChangeReport . filterM monitorChanged

monitorChanged :: StatusMonitor -> STM Bool
monitorChanged m = (/= oldStatus m) <$> currentStatus m

toChangeReport :: StatusMonitor -> STM (Request, ResourceStatus)
toChangeReport m = (request m,) . currentStatus $ m

currentStatus :: StatusMonitor -> STM ResourceStatus
currentStatus = fmap refValue . readTVar . monitor

updateMonitors :: [StatusMonitor] -> STM [StatusMonitor]
updateMonitors = fmap (filter (oldStatus /= Deleted)) . mapM updateOldStatus

updateOldStatus :: StatusMonitor -> STM StatusMonitor
updateOldStatus m = do
  update <- currentStatus m
  return m { oldStatus = update }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    paths = map (requestPath . request) ms
    assList = zip paths ms
  in
    Map.fromList assList
