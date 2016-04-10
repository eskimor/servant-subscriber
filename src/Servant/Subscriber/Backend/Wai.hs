
-- | Backend of accessing a wai server:
module Servant.Subscriber.Backend.Wai where

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


instance Backend Wai.Application where
  requestResource app req c = void $ app req (sendResponse c path event)
    where
      path = Path . T.decodeUtf8 $ Wai.rawPathInfo req


fromWaiResponse :: Wai.Response -> Response
fromWaiResponse (Wai.ResponseBuilder status headers builder)=
    writeResponse c Response {
    httpStatus = fromHTTPStatus status
    , httpHeaders = fromHTTPHeaders headers
    , httpBody = JSONBody . B.toByteString $ builder
    }
sendResponse _ = error "I am sorry - this 'Response' type is not yet implemented in servant-subscriber!"
