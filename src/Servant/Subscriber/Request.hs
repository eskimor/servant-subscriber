{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Request where

import qualified Blaze.ByteString.Builder        as B
import           Control.Concurrent.STM.TVar     (TVar)
import           Data.Aeson
-- import           Data.Aeson.Encode.Builder       as AesonBuilder
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
import           Servant.Subscriber.Subscribable
import           Servant.Subscriber.Types




{--| We don't use Network.HTTP.Types here, because we need FromJSON instances, which can not
     be derived for 'ByteString'
--}
type RequestHeader = (Text, Text)
type RequestHeaders = [RequestHeader]

-- | Any message from the client is a 'Request':
-- Currently you can only subscribe to 'GET' method endpoints.
data Request =
    Subscribe !HttpRequest
  | Unsubscribe !Path
  deriving Generic

instance FromJSON Request
instance ToJSON Request


data HttpRequest = HttpRequest {
  httpPath    :: !Path
, httpHeaders :: RequestHeaders
, httpQuery   :: H.QueryText
, httpBody    :: RequestBody
} deriving Generic

instance FromJSON HttpRequest
instance ToJSON HttpRequest

newtype RequestBody = RequestBody Value

instance FromJSON RequestBody where
  parseJSON = return . RequestBody

instance ToJSON RequestBody where
  toJSON (RequestBody v) = v
  -- toEncoding (RequestBody v) = Encoding . AesonBuilder.encodeToBuilder

toHTTPHeader :: RequestHeader -> H.Header
toHTTPHeader = bimap (Case.mk . T.encodeUtf8) T.encodeUtf8

toHTTPHeaders :: RequestHeaders -> H.RequestHeaders
toHTTPHeaders = map toHTTPHeader

requestPath :: Request -> Path
requestPath (Subscribe req)    = httpPath req
requestPath (Unsubscribe path) = path
