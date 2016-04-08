module Servant.Subscriber.Client where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.STM.TVar     (TVar)
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
import           Network.WebSockets.Connection   as WS
import           Servant.Server

import           Data.Bifunctor
import           Servant.Subscriber
import           Servant.Subscriber.Request
import           Servant.Subscriber.Subscribable

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

data Client = Client {
    watches :: [(Url, TVar ResourceStatus)]
  }
