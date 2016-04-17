{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Servant.Subscriber where

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar, modifyTVar')
import Control.Concurrent.STM (STM)

import           Data.Aeson
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           GHC.Generics
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import Network.URI (URI(..))

import Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
import           Network.WebSockets.Connection as WS
import           Servant.Server
import Servant.Utils.Links (IsElem, HasLink, MkLink, safeLink)
import Control.Monad


import System.Mem.Weak
import Servant.Subscriber.Subscribable
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import GHC.Conc

type ClientId = Int
type ReferenceCount = Int
type Revision = Int
newtype Path = Path Text deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)


type ResourceStatusMap = Map Path (TVar (RefCounted ResourceStatus))

data ResourceStatus = Modified Revision -- |< Watching for 'Modified' implies watching for 'Deleted'
  | Deleted
  deriving (Eq, Show)

data RefCounted a = RefCounted {
 refCount :: ReferenceCount
, refValue :: a
}

instance Functor RefCounted where
  fmap f (RefCounted c v) = RefCounted c (f v)

data Subscriber = Subscriber {
  {--
    In order to improve multithread performance even more, this Map could be replaced with some hierarchical Map data structure:

    > type TreeMap = Either (TVar (Map PathPiece MapElem)) ResourceStatus
    > Map PathPiece TreeMap
    with PathPiece being a part of the URI path:

    > 'families/1200/invitations'

    here families, 1200 and invitations. This way Map updates are more local and not every change to some element in the tree
    needs to update the same toplevel Map TVar. This could improve scalability, if multi processor performance is not good enough.
  --}
  subState :: !(TVar ResourceStatusMap)
}

{--|
  Notify the subscriber about a changed resource.
  You have to provide a typesafe link to the changed resource. Only
  Symbols and Captures are allowed in this link.

  You need to provide a proxy to the API too. This is needed to check that the endpoint is valid
  and points to a 'Subscribable' resource.

  In addition the event you want to notify about has to be provided via a proxy.

  One piece is still missing - we have to fill out captures, that's what the getLink parameter is
  for: You will typicall provide a lamda there providing needed parameters.

  TODO: Example!
--}
notify :: (IsElem endpoint api, HasLink endpoint
  , IsValidEndpoint endpoint, IsSubscribable endpoint api)
  => Subscriber
  -> Proxy api
  -> Event
  -> Proxy endpoint
  -> (MkLink endpoint -> URI)
  -> STM ()
notify subscriber pApi event pEndpoint getLink = do
  let resource = Path . T.pack . uriPath $ getLink (safeLink pApi pEndpoint)
  modifyState event resource subscriber



-- | Get a ResourceState - it will be created when not present
getState :: Path -> Subscriber -> STM (TVar (RefCounted ResourceStatus))
getState p s = do
      states <- readTVar $ subState s
      let mState = Map.lookup p states
      case mState of
        Nothing -> do
           state <- newTVar $ RefCounted 1 (Modified 0)
           writeTVar (subState s) $ Map.insert p state states
           return state
        Just state -> return state

-- | Unget a previously got ResourceState - make sure you match every call to getState with a call to unGetState!
unGetState :: Path -> TVar (RefCounted ResourceStatus) -> Subscriber -> STM ()
unGetState p tv s = do
  v <- (\a -> a { refCount = refCount a - 1}) <$> readTVar tv
  if refCount v == 0
    then modifyTVar' (subState s) (Map.delete p)
    else writeTVar tv v

-- | Modify a ResourceState if it is present in the map, otherwise do nothing.
modifyState :: (ResourceStatus -> ResourceStatus) -> Path -> Subscriber -> STM ()
modifyState update p s = do
  rMap <- readTVar (subState s)
  maybe (return ()) -- Nothing to modify - fine!
        (`modifyTVar'` fmap update) $ Map.lookup p rMap

type Event = ResourceStatus -> ResourceStatus

deleteEvent :: Event
deleteEvent (Modified _) = Deleted
deleteEvent _ = error "Resource can not be deleted - it does not exist!"

modifyEvent :: Event
modifyEvent (Modified n) = Modified (n + 1)
modifyEvent _ = error "Resource can not be modified - it does not exist!"
