{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Client where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.STM          (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad                   (void)
import           Data.Aeson
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
import qualified Network.Wai                     as Wai
import qualified Network.Wai.Internal            as Wai
import           Network.WebSockets.Connection   as WS
import           Servant.Server

import           Data.Bifunctor
import           Servant.Subscriber
import           Servant.Subscriber.Request
import           Servant.Subscriber.Subscribable

data RequestError = ResourceNotAvailable | SubscriptionNotAllowed deriving (Show, Generic)

data Client = Client {
    watches       :: !(TVar [StatusMonitor])
  , readRequest   :: !(IO Request)
  , writeResponse :: !(Response -> IO ())
  }

data StatusMonitor = StatusMonitor {
  path      :: !Path
, request   :: !Request
, monitor   :: !(TVar ResourceStatus)
, oldStatus :: !ResourceStatus
}

run :: Client -> IO ()
run c = undefined


monitorChanges :: Wai.Application -> Client -> IO ()
monitorChanges c = do
  changes <- atomically $ getChanges c
  error "Not yet implemented"

getChanges :: Client -> STM [(Path, ResourceStatus)]
getChanges c = do
      ws <- readTVar $ watches c
      newValues <- mapM (readTVar . monitor) ws
      let oldValues = map oldStatus ws
      let changed = zipWith (/=) oldValues newValues
      let preResult = zipWith (\sm new -> (path sm, new)) ws newValues
      let onlyChanges = filterByList changed preResult
      let result = filter (ignoreWaitForCreate . snd) onlyChanges
      if null result -- No changes :-(
        then retry
        else do
          writeTVar (watches c)
              $ filter (stillWatching . oldStatus)
              . zipWith updateOldStatus newValues $ ws
          return result
  where
    ignoreWaitForCreate :: ResourceStatus -> Bool
    ignoreWaitForCreate (WaitForCreate _) = False
    ignoreWaitForCreate _ = True

    filterByList bools vals = map snd . filter fst $ zip bools vals

    updateOldStatus :: ResourceStatus -> StatusMonitor -> StatusMonitor
    updateOldStatus new s = s { oldStatus = new}

    stillWatching :: ResourceStatus -> Bool
    stillWatching (WaitForCreate _) = True -- Just a new waiting client appeared
    stillWatching (Modified _) =  True -- We are watching for any modification
    stillWatching _ = False -- All other events are one-shot
