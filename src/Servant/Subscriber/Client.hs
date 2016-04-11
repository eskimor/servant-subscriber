{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Client where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.STM          (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import Control.Concurrent.Async
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


data Client = Client {
    watches       :: !(TVar [StatusMonitor])
  , readRequest   :: !(IO Request)
  , writeResponse :: !(Response -> IO ())
  }

data StatusMonitor = StatusMonitor {
  request   :: !Request
, monitor   :: !(TVar ResourceStatus)
, oldStatus :: !ResourceStatus
}

run :: Backend backend => backend -> Subscriber -> Client -> IO ()
run b s c = do
    let work = race (monitorChanges b c) (handleRequests s c)
    let cleanup = atomically $ do
      monitors <- readTVar (watches c)
      mapM_ (releaseWaitForCreate s) monitors
    finally work cleanup

handleRequests :: Subscriber -> Client -> IO ()
handleRequests s c = forever $ do
    req <- readRequest c
    atomically $ case rAction req of
      Subscribe event -> case event of
        CreatedEvent -> handleSubscribeCreated s c req
        _ -> handleSubscribe s c req
      Unsubscribe event -> case event of
        CreatedEvent -> handleUnsubscribeCreated s c req
        _ -> handleUnsubscribe s c req

handleSubscribe :: Subscriber -> Client -> Request -> STM
handleSubscribe sub c req = do
  let path = httpPath . httpData $ req
  lookupState

releaseWaitForCreate :: Subscriber -> StatusMonitor -> STM ()
releaseWaitForCreate subscriber status = do
  let path = httpPath . httpData . request $ status
  v <- readTVar (monitor status)
  case v of
    WaitForCreate _ -> alterState handleDeadWaitingClient path s
    _ -> return ()



monitorChanges :: Backend backend => backend -> Client -> IO ()
monitorChanges b c = forever $ do
    changes <- atomically $ getChanges c
    mapM_ (sendUpdate b (writeResponse c)) changes

sendUpdate :: Backend backend => backend -> (Response -> IO ()) -> (Request, EventName) -> IO ()
sendUpdate b sendResponse (req, DeletedEvent) = sendResponse . DeletedResponse . httpPath . httpData $ req
sendUpdate b sendResponse (req, event) = sendRequest backend
  $ \ httpResponse -> do
    let path = httpPath . httpData $ req
    sendResponse $ Response path event httpResponse
    return ResponseReceived
  return ()

getChanges :: Client -> STM [(Request, EventName)]
getChanges c = do
      ws <- readTVar $ watches c
      newValues <- mapM (readTVar . monitor) ws
      let oldValues = map oldStatus ws
      let changed = zipWith (/=) oldValues newValues
      let preResult = zipWith (\sm new -> (request sm, new)) ws newValues
      let onlyChanges = filterByList changed preResult
      let result = mapMaybe (fmap toEventName) onlyChanges
      if null result -- No real changes :-(
        then retry
        else do
          writeTVar (watches c)
              $ filter (stillWatching . oldStatus)
              . zipWith updateOldStatus newValues $ ws
          return result
  where
    filterByList bools vals = map snd . filter fst $ zip bools vals

    updateOldStatus :: ResourceStatus -> StatusMonitor -> StatusMonitor
    updateOldStatus new s = s { oldStatus = new}

    stillWatching :: ResourceStatus -> Bool
    stillWatching (WaitForCreate _) = True -- Just a new waiting client appeared
    stillWatching (Modified _) =  True -- We are watching for any modification
    stillWatching _ = False -- All other events are one-shot
