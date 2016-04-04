module Servant.Subscriber where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)

type ClientId = Int
type Url = Text
data ResponseData = ResponseData (forall v. ToJSON v => v) deriving Generic

data Subscriber api =
  Subscriber {
    --| Store clients by some unique id - this can be used
    --| to identify a client and e.g. close it's connection when its account got deleted.
    clients :: IntMap Client
  , server :: Server api
  , subscribers :: Map ChangeEvent [ClientId]
  -- | When should client connections be dropped?
  , connectionTimeout :: NominalDiffTime
  -- | You can register a handler for getting informed if a client left:
  , clientDroppedHandler :: ClientId -> IO ()
  }

data Client =
  Client {
    -- | When did we receive our last pong - close connections after some timeout.
    lastPong :: UTCTime
    -- | Needed - if we want immediate cleanup on connection drop:
  , subscriptions :: [ChangeEvent]
  }

data EventName = CreateEvent | ModifiedEvent | DeletedEvent deriving (Eq, Ord, Show)

data ChangeEvent = ChangeEvent {
    resource :: Url
  , event :: EventName
  } deriving (Eq, Ord, Show)

data Response = EventResponse {
    , responseResource :: Url
    , responseEventName :: EventName
    -- | Response data - Nothing on DeletedEvent:
    , responseData :: Maybe ResponseData
    }
  | ErrorResponse ServantErr
  deriving Generic

data SubscribeAction = SubscribeCreate | SubscribeModify | SubscribeDelete

data Request = Request {

}
