{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}

module Servant.Subscriber where

import Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           Data.Map                      (Map)
import           Data.Text                     (Text)
import           Data.Time
import           Network.WebSockets.Connection as WS
import           Servant.Server

type ClientId = Int
type Url = Text
type ResourceId = URI
data ResponseData = forall v. ToJSON v => ResponseData v deriving Generic
type Revision = Int

{--
  ResourceStatus could only be the revision integer and creation and deletion could be solely represented
  by adding/removing it to our resources map. The problem is, this does not scale very well, as then _every_ update wakes up
  _all_ clients and they have to check whether they are affected by the change! (They are using STM's 'retry'.)

  To avoid that, we use the following mechanism: Clients are allowed to create elements in our ResourceMap too:
  If a client wants to subscribe to a 'CreateEvent' event, it will just insert the corresponding entry marked as 'Deleted'.
  Then the client can just monitor the entry it just created and on creation the server will simply set it to 'Created' instead
  of adding a new entry. If no client waited for the creation, then obviously the server will create a new entry, marked 'Created'.

  The more interesting part is when a resource gets deleted: It will obviously get removed from the map - but monitoring clients
  would not notice that, so in addition the status will be changed to 'Deleted'. Once all clients have stopped monitoring this resource
  it will be garbage collected as it is no longer present in the map.

  So a client will query the map in an atomic operation - inserting any nodes it wants to monitor which are not yet present. Afterwards
  it can start a second atomic operation querying the previously retrieved resources.
  This second atomic operation can then simply be 'retry'ed to get notified about any changes - changes to the map will no longer wake the client!
  This is ok as there are no changes to the map that could be of any interest for the client, because we have our 'Deleted' and 'Created' states.
  Uuuuh yeah - this took stupid me some time, but now I really like this solution :-)

  Thanks to STM we get a performant subscriber with no need for any weak reference tricks- we can fully rely on automatic garbage collection.
--}
data ResourceStatus = Deleted | Created | Modified Revision

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
  subResources :: !TVar (Map ResourceId (TVar ResourceStatus))
}

update :: (IsElem api endpoint, HasLink endpoint, SubscriptionOk api endpoint eventName)
  => Proxy api
  -> Proxy (eventName :: EventName)
  -> Proxy endpoint
  -> Subscriber
  -> (MkLink endpoint -> URI)
  -> STM ()
update pApi pEventName pEndpoint getLink = do
  let resource = getLink (toLink pEndpoint)
  case fromEventNameProxy pEventName of
    CreateEvent -> modifyTVar (subResources )


-- data Subscriber api = Subscriber {
--     clients              :: IntMap Client
--   , server               :: Server api
--   , subscribers          :: TVar (Map ChangeEvent [ClientId])
--   -- | When should client connections be dropped?
--   , connectionTimeout    :: NominalDiffTime
--   -- | You can register a handler for getting informed if a client left:
--   , clientDroppedHandler :: ClientId -> IO ()
--   }



data EventName = CreateEvent | ModifiedEvent | DeletedEvent deriving (Eq, Ord, Show)

class EventNameFromProxy (a :: EventName) where
  fromEventNameProxy :: Proxy a -> a

instance EventNameFromProxy CreateEvent where
  fromEventnameProxy _ = CreateEvent

instance EventNameFromProxy ModifiedEvent where
  fromEventnameProxy _ = ModifiedEvent

instance EventNameFromProxy DeletedEvent where
  fromEventnameProxy _ = DeletedEvent

data ChangeEvent = ChangeEvent {
    resource :: Url
  , event    :: EventName
  } deriving (Eq, Ord, Show)

data EventMessage = EventMessage {
      responseResource  :: Url
    , responseEventName :: EventName
    -- | Response data - Nothing on DeletedEvent:
    , responseData      :: Maybe ResponseData
    }
  | EventError Url EventName ServantErr
  deriving Generic

data SubscribeAction = SubScribe EventName | Unsubscribe EventName

data Request = Request {
  subscribe :: SubscribeAction
, resource :: Url
,
}
