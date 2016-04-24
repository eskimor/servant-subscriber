module Servant.Subscriber.Client where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.Async
import           Control.Concurrent.STM          (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString                 as BS
import qualified Data.CaseInsensitive            as Case
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.Map                        (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as T
import           Data.Time
import           GHC.Generics
import qualified Network.HTTP.Types              as H
import           Network.WebSockets.Connection   as WS
import           Servant.Server

import           Control.Exception
import           Control.Monad
import           Servant.Subscriber              as S
import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response     as Resp
import           Servant.Subscriber.Subscribable
import           Servant.Subscriber.Types

type ClientMonitors = Map Path StatusMonitor

data Client = Client {
    monitors      :: !(TVar ClientMonitors)
  , readRequest   :: !(IO Request)
  , writeResponse :: !(Response -> IO ())
  }

data StatusMonitor = StatusMonitor {
  request   :: !HttpRequest
, monitor   :: !(TVar (RefCounted ResourceStatus))
, oldStatus :: !ResourceStatus
}

run :: Backend backend => backend -> Subscriber -> Client -> IO ()
run b sub c = do
  let
    work    = race_ (runMonitor b c) (handleRequests b sub c)
    cleanup = atomically $ do
      ms <- readTVar (monitors c)
      mapM_ (unsubscribeMonitor sub) ms
  finally work cleanup

unsubscribeMonitor :: Subscriber -> StatusMonitor -> STM ()
unsubscribeMonitor sub m =
  let
    path = httpPath . request $ m
    mon = monitor m
  in
    unsubscribe path mon sub

subscribeMonitor :: Subscriber -> HttpRequest -> Client -> STM ()
subscribeMonitor sub req c = do
  let path = httpPath req
  tState <- subscribe path sub
  stateVal <- refValue <$> readTVar tState
  modifyTVar' (monitors c) $ Map.insert path (StatusMonitor req tState stateVal)

handleRequests :: Backend backend => backend -> Subscriber -> Client -> IO ()
handleRequests b sub c = forever $ do
    req <- readRequest c
    case req of
      Subscribe req    -> handleSubscribe b sub c req
      Unsubscribe path -> handleUnsubscribe b sub c path

handleSubscribe :: Backend backend => backend -> Subscriber -> Client -> HttpRequest -> IO ()
handleSubscribe b sub c req = void $ requestResource b req $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let path = httpPath req
    let isGoodStatus = status >= 200 && status < 300 -- For now we only accept success
    if isGoodStatus
      then
        writeResponse c <=< atomically $ do
          ms <- readTVar (monitors c)
          case Map.lookup path ms of
            Just _  -> return $ RequestError (Subscribe req) AlreadySubscribed
            Nothing -> do
              subscribeMonitor sub req c
              return $ Resp.Modified path httpResponse
      else
        writeResponse c $ RequestError (Subscribe req) (ServerError httpResponse)
    return ResponseReceived

handleUnsubscribe :: Backend backend => backend -> Subscriber -> Client -> Path -> IO ()
handleUnsubscribe b sub c path = writeResponse c <=< atomically $ do
    ms <- readTVar (monitors c)
    case Map.lookup path ms of
      Nothing -> return $ RequestError (Unsubscribe path) NoSuchSubscription
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
      ms <- Map.elems <$> readTVar (monitors c)
      result <- getChanges ms
      if null result
        then retry
        else do
          writeTVar (monitors c) . monitorsFromList <=< updateMonitors $ ms
          return result


getChanges :: [StatusMonitor] -> STM [(HttpRequest, ResourceStatus)]
getChanges = mapM toChangeReport <=< filterM monitorChanged

monitorChanged :: StatusMonitor -> STM Bool
monitorChanged m = (/= oldStatus m) <$> currentStatus m

toChangeReport :: StatusMonitor -> STM (HttpRequest, ResourceStatus)
toChangeReport m = (,) (request m) <$> currentStatus m

currentStatus :: StatusMonitor -> STM ResourceStatus
currentStatus = fmap refValue . readTVar . monitor

updateMonitors :: [StatusMonitor] -> STM [StatusMonitor]
updateMonitors = fmap (filter (\m -> oldStatus m /= S.Deleted)) . mapM updateOldStatus

updateOldStatus :: StatusMonitor -> STM StatusMonitor
updateOldStatus m = do
  update <- currentStatus m
  return m { oldStatus = update }

monitorsFromList :: [StatusMonitor] -> ClientMonitors
monitorsFromList ms = let
    paths = map (httpPath . request) ms
    assList = zip paths ms
  in
    Map.fromList assList
