{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}



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

import Servant.Subscriber.Subscribable

type ClientId = Int
type ReferenceCount = Int
type Url = Text
data ResponseData = forall v. ToJSON v => ResponseData v

type Revision = Int
newtype Path = Path Text deriving (Eq, Ord, Show)
type ResourceStatusMap = Map Path (TVar ResourceStatus)

{--
  ResourceStatus could only be the revision integer and creation and deletion could be solely represented
  by adding/removing it to our resources map. The problem is, this does not scale very well, as then _every_ update wakes up
  _all_ clients and they have to check whether they are affected by the change! (They are using STM's 'retry'.)

  To avoid that, we use the following mechanism: Clients are allowed to create elements in our state map too:
  If a client wants to subscribe to a 'CreateEvent' event, it will just insert the corresponding entry marked as 'WaitForCreate 1'.
  Any additional interested client will increment the value.
  Then the client can just monitor the entry it just created and on creation the server will simply set it to 'Created' instead
  of adding a new entry. If no client waited for the creation, then obviously the server will create a new entry, marked 'Created'.

  If a client disconnects before the resource was created, an exception handler will decrement the 'WaitForCreate' value and if it is 0 will remove the entry from the map.
  This way we make sure to not gather stale entries.

  The more interesting part is when a resource gets deleted: It will obviously get removed from the map - but monitoring clients
  would not notice that, so in addition the status will be changed to 'Deleted'. Once all clients have stopped monitoring this resource
  it will be garbage collected as it is no longer present in the map.

  So a client will query the map in an atomic operation - inserting any nodes it wants to monitor which are not yet present. Afterwards
  it can start a second atomic operation querying the previously retrieved resources.
  This second atomic operation can then simply be 'retry'ed to get notified about any changes - changes to the map will no longer wake the client!
  This is ok as there are no changes to the map that could be of any interest for the client.

  Thanks to STM we get a performant subscriber with no need for any weak reference tricks- we can fully rely on automatic garbage collection (and a little reference counter).
--}
data ResourceStatus = WaitForCreate ReferenceCount | Created | Modified Revision | Deleted deriving (Eq, Show)

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
  , IsValidEndpoint endpoint, IsSubscribable endpoint api eventName
  , EventNameFromProxy eventName)
  => Proxy api
  -> Proxy (eventName :: EventName)
  -> Proxy endpoint
  -> Subscriber
  -> (MkLink endpoint -> URI)
  -> STM ()
notify pApi pEventName pEndpoint subscriber getLink = do
  let resource = Path . T.pack . uriPath $ getLink (safeLink pApi pEndpoint)
  case fromEventNameProxy pEventName of
    CreatedEvent  -> alterState handleCreate resource subscriber
    DeletedEvent  -> alterState handleDelete resource subscriber
    ModifiedEvent -> alterState handleModify resource subscriber


alterState :: (ResourceStatus -> ResourceStatus) -> Path -> Subscriber -> STM ()
alterState update p s = do
    rMap <- readTVar (subState s)
    let mtOld = Map.lookup p rMap
    -- WARNING: If we ever change the handle functions to not error out - we need to make sure to not add a WaitForCreate to the map!
    tStatus <- maybe (newTVar (WaitForCreate 0)) return mtOld

    modifyTVar' tStatus update
    new <- readTVar tStatus
    case mtOld of
      Nothing -> writeTVar (subState s) (Map.insert p tStatus rMap)
      Just _ ->  when (new == Deleted) $ writeTVar (subState s) (Map.delete p rMap)

handleCreate :: ResourceStatus -> ResourceStatus
handleCreate (WaitForCreate _) = Created
handleCreate _ = error "Resource can not be created - it already exists!" -- If this error ever gets removed - check alterState!

handleDelete :: ResourceStatus -> ResourceStatus
handleDelete (Modified _) = Deleted
handleDelete Created = Deleted
handleDelete _ = error "Resource can not be deleted - it does not exist!"

handleModify :: ResourceStatus -> ResourceStatus
handleModify (Modified n) = Modified (n + 1)
handleModify Created = Modified 1
handleModify _ = error "Resource can not be modified - it does not exist!"



data SubscribeAction = SubScribe EventName | Unsubscribe EventName

-- | Any message from the client is a 'Request':
data Request = Request {
  subscribe :: SubscribeAction
, resource :: Url
}

-- | Any message from the server is a Response.
data Response = Response {
      responseResource  :: Url
    , responseEventName :: EventName
    -- | Response data - Nothing on DeletedEvent:
    , responseData      :: Maybe ResponseData
    }
  | ServerError Url EventName ServantErr
  | RequestError Request RequestError
  deriving Generic

data RequestError = ResourceNotAvailable | SubscriptionNotAllowed deriving (Show, Generic)
