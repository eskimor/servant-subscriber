{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Servant.Subscriber.Types where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              readTVar, writeTVar)
import           Control.Monad
import           Control.Monad.IO.Class
import Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           GHC.Conc
import           GHC.Generics
import           Network.URI                 (URI (..), pathSegments)
import Servant.Utils.Links (IsElem, HasLink, MkLink, safeLink)
import Data.String (IsString, fromString)
import System.FilePath.Posix (splitPath)

import Servant.Subscriber.Subscribable

newtype Path = Path [Text] deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

instance IsString Path where
  fromString = Path . map T.pack . splitPath

type ReferenceCount = Int
type Revision = Int
type ResourceStatusMap = Map Path (TVar (RefCounted ResourceStatus))

data ResourceStatus =
    Modified Revision -- |< Watching for 'Modified' implies watching for 'Deleted'
  | Deleted
  deriving (Eq, Show)

data RefCounted a = RefCounted {
  refCount :: ReferenceCount
, refValue :: a
}

instance Functor RefCounted where
  fmap f (RefCounted c v) = RefCounted c (f v)

type LogRunner = forall m a. MonadIO m => LoggingT m a -> m a

data Subscriber api = Subscriber {
  {--
    In order to improve multithread performance even more, this Map could be replaced with some hierarchical Map data structure:

    > type TreeMap = Either (TVar (Map PathPiece MapElem)) ResourceStatus
    > Map PathPiece TreeMap
    with PathPiece being a part of the URI path:

    > 'families/1200/invitations'

    here families, 1200 and invitations. This way Map updates are more local and not every change to some element in the tree
    needs to update the same toplevel Map TVar. This could improve scalability, if multi processor performance is not good enough.
  --}
  subState   :: !(TVar ResourceStatusMap)
-- On which path do we wait for WebSocket connections?
, entryPoint :: !Path
, runLogging :: LogRunner
}

data Event = DeleteEvent | ModifyEvent deriving (Eq)

{--|
  Notify the subscriber about a changed resource.
  You have to provide a typesafe link to the changed resource. Only
  Symbols and Captures are allowed in this link.

  You need to provide a proxy to the API too. This is needed to check that the endpoint is valid
  and points to a 'Subscribable' resource.

  One piece is still missing - we have to fill out captures, that's what the getLink parameter is
  for: You will typicall provide a lamda there providing needed parameters.

  TODO: Example!
--}
notify :: forall api endpoint. (IsElem endpoint api, HasLink endpoint
  , IsValidEndpoint endpoint, IsSubscribable endpoint api)
  => Subscriber api
  -> Event
  -> Proxy endpoint
  -> (MkLink endpoint -> URI)
  -> STM ()
notify subscriber event pEndpoint getLink = do
  let mkPath = Path . map T.pack . pathSegments . getLink
  let resource = mkPath $ safeLink (Proxy :: Proxy api) pEndpoint
  modifyState event resource subscriber

-- | Version of notify that lives in 'IO' - for your convenience.
notifyIO :: forall api endpoint. (IsElem endpoint api, HasLink endpoint
  , IsValidEndpoint endpoint, IsSubscribable endpoint api)
  => Subscriber api
  -> Event
  -> Proxy endpoint
  -> (MkLink endpoint -> URI)
  -> IO ()
notifyIO subscriber event pEndpoint getLink = atomically $ notify subscriber event pEndpoint getLink


-- | Subscribe to a ResourceStatus - it will be created when not present
subscribe :: Path -> Subscriber api -> STM (TVar (RefCounted ResourceStatus))
subscribe p s = do
      states <- readTVar $ subState s
      let mState = Map.lookup p states
      case mState of
        Nothing -> do
           state <- newTVar $ RefCounted 1 (Modified 0)
           writeTVar (subState s) $ Map.insert p state states
           return state
        Just state -> do
           modifyTVar' state $ \s -> s { refCount = refCount s + 1}
           return state

-- | Unget a previously got ResourceState - make sure you match every call to subscribe with a call to unsubscribe!
unsubscribe :: Path -> TVar (RefCounted ResourceStatus) -> Subscriber api -> STM ()
unsubscribe p tv s = do
  v <- (\a -> a { refCount = refCount a - 1}) <$> readTVar tv
  if refCount v == 0
    then modifyTVar' (subState s) (Map.delete p)
    else writeTVar tv v

-- | Modify a ResourceState if it is present in the map, otherwise do nothing.
modifyState :: Event -> Path -> Subscriber api -> STM ()
modifyState event p s = do
  rMap <- readTVar (subState s)
  case Map.lookup p rMap of
    Nothing -> return ()
    Just refStatus -> do
      modifyTVar refStatus $ fmap (eventHandler event)
      when (event == DeleteEvent) $ modifyTVar (subState s) (Map.delete p)

eventHandler :: Event -> ResourceStatus -> ResourceStatus
eventHandler ModifyEvent = doModify
eventHandler DeleteEvent = doDelete

doDelete :: ResourceStatus -> ResourceStatus
doDelete (Modified _) = Deleted
doDelete _ = error "Resource can not be deleted - it does not exist!"

doModify :: ResourceStatus -> ResourceStatus
doModify (Modified n) = Modified (n + 1)
doModify _ = error "Resource can not be modified - it does not exist!"

toSegments :: Path -> [Text]
toSegments (Path xs) = xs
