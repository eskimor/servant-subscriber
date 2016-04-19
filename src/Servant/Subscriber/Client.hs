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
import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response
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
    case rAction req of
      Subscribe   -> handleSubscribe b sub c req
      Unsubscribe -> handleUnsubscribe b sub c req

handleSubscribe :: Backend backend => backend -> Subscriber -> Client -> Request -> IO ()
handleSubscribe b sub c req = sendRequest b req $ \ httpResponse -> do
    let status = statusCode . httpStatus $ httpResponse
    let path = requestPath req
    let isGoodStatus = status >= 200 && status < 300 -- For now we only accept success
    if isGoodStatus
      then
        writeResponse c <<= atomically $ do
          let path = requestPath req
          case Map.lookup path (monitors c) of
            Just _  -> return $ RequestError path SubscribeAction AlreadySubscribed
            Nothing -> do
              subscribeMonitor sub req c
              return $ ModifiedResponse path httpResponse
      else
        writeResponse c $ RequestError path SubscribeAction (ServerError httpResponse)

handleUnsubscribe :: Backend backend => backend -> Subscriber -> Client -> Request -> IO ()
handleUnsubscribe b sub c req = writeResponse c <<= atomically $ do
    let path = requestPath req
    ms <- readTVar (monitors c)
    case Map.lookup path ms of
      Nothing -> return $ RequestError path Unsubscribe NoSuchSubscription
      Just m -> do
        unsubscribeMonitor s m
        writeTVar (monitors c) $ Map.delete path
        return $ Unsubscribed path

monitorChanges :: Backend backend => backend -> Client -> IO ()
monitorChanges b c = forever $ do
    changes <- atomically $ getChanges c
    mapM_ (sendUpdate b (writeResponse c)) changes

sendUpdate :: Backend backend => backend -> (Response -> IO ()) -> (Request, ResourceStatus) -> IO ()
sendUpdate b sendResponse (req, Deleted)    = sendResponse $ DeletedResponse (requestPath req)
sendUpdate b sendResponse (req, Modified _) = sendServerResponse backend req sendResponse

sendServerResponse :: Backend backend => backend -> Request -> (Response -> IO ()) -> IO ()
sendServerResponse b req sendResponse = void $ sendRequest b (httpData req)
  $ \ httpResponse -> do
    let path = requestPath req
    sendResponse $ ModifiedResponse path httpResponse
    return ResponseReceived

-- | TODO: Fixme: We are a map and no longer a list - state update is broken!
getChanges :: Client -> STM [(Request, ResourceStatus)]
getChanges c = do
      ms <- elems <$> readTVar (monitors c)
      newValues <- mapM (fmap refValue . readTVar . monitor) ms
      let oldValues = map oldStatus ms
      let changed = zipWith (/=) oldValues newValues
      let requests = map request ms
      let requestValues = zip requests newValues
      let result = filterByList changed requestValues
      if null result -- No real changes :-(
        then retry
        else do
          writeTVar (monitors c)
              $ filter (stillWatching . oldStatus)
              . zipWith updateOldStatus newValues $ ms
          return result
  where
    filterByList bools vals = map snd . filter fst $ zip bools vals

    updateOldStatus :: ResourceStatus -> StatusMonitor -> StatusMonitor
    updateOldStatus new s = s { oldStatus = new}

    stillWatching :: ResourceStatus -> Bool
    stillWatching (Modified _) =  True -- We are watching for any modification
    stillWatching Deleted = False
