module Servant.Subscriber.Client where


import Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           Data.Map                      (Map)
import           Data.Text                     (Text)
import           Data.Time
import           Network.WebSockets.Connection as WS
import           Servant.Server

data Client =
  Client {
    -- | When did we receive our last pong - close connections after some timeout.
    lastPong      :: TVar UTCTime
    -- | Needed - if we want immediate cleanup on connection drop:
  , subscriptions :: TVar [ChangeEvent]
  , connection    :: WS.Connection
  }
