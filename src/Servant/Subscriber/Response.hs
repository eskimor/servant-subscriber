{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Response where


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

type ResponseHeader = RequestHeader
type ResponseHeaders = RequestHeaders

-- | Any message from the server is a Response.
data Response =
    Response !Path !EventName !HttpResponse
  | ServerError !Path !ServantErr
  | RequestError !Path !SubscribeAction !RequestError
  deriving Generic

instance ToJSON Response

data HttpResponse = HttpResponse {
  httpStatus  :: !Status
, httpHeaders :: !ResponseHeaders
, httpBody    :: JSONBody
} deriving Generic

instance ToJSON HttpResponse


data Status = Status {
  statusCode    :: !Int
, statusMessage :: !Text
} deriving Generic

instance ToJSON Status

data JSONBody = JSONBody Builder

instance ToJSON JSONBody where
  toJSON (JSONBody b) = parse value (B.toLazyByteString b)
  -- toEncoding = Encoding

fromHTTPHeader :: H.Header -> ResponseHeader
fromHTTPHeader = bimap (Case.original . T.decodeUtf8) T.decodeUtf8

fromHTTPHeaders :: H.ResponsHeaders -> ResponseHeaders
fromHTTPHeaders = map fromHTTPHeader

fromHTTPStatus :: H.Status -> Status
fromHTTPStatus s = Status {
  statusCode = H.statusCode s
, statusMessage = T.decodeUtf8 . H.statusMessage $ s
}
