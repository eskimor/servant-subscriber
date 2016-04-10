-- | A backend for subscriber allows us to query a server and get a response.
-- | We mimick Wai's interface but with 'Request' and 'Response' types that match our needs.
-- | Our only backend right now actually is implemented via the WAI Application interface, see: "Servant.Subscriber.Backend.Wai".
module Servant.Subscriber.Backend where

import qualified Blaze.ByteString.Builder      as B
import           Control.Concurrent.STM        (STM, atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad                 (void)
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.CaseInsensitive          as Case
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as IntMap
import           Data.Map                      (Map)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as T
import           Data.Time
import           GHC.Generics
import qualified Network.HTTP.Types            as H
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Internal          as Wai
import           Network.WebSockets.Connection as WS
import           Servant.Server

import           Servant.Subscriber.Request
import           Servant.Subscriber.Response

data ResponseReceived = ResponseReceived

type Application = HttpRequest -> (HttpResponse -> IO ResponseReceived) -> IO ResponseReceived

class Backend a where
  requestResource :: a -> Application
