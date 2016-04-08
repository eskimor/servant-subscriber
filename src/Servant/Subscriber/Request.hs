{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Request where

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
import           Servant.Subscriber.Subscribable


{--| We don't use Network.HTTP.Types here, because we need FromJSON instances, which can not
     be derived for 'ByteString'
--}
type RequestHeader = (Text, Text)
type RequestHeaders = [RequestHeader]

-- | Any message from the client is a 'Request':
-- Currently you can only subscribe to 'GET' method endpoints.
data Request = Request {
  rSubscribe :: SubscribeAction
, rPath      :: [Text]
, rHeaders   :: RequestHeaders
, rQuery     :: H.QueryText
, rBody      :: Text
} deriving Generic

instance FromJSON Request

data SubscribeAction = SubScribe EventName | Unsubscribe EventName deriving (Generic, Show)

instance FromJSON SubscribeAction

toWaiRequest :: Request -> Wai.Request
toWaiRequest r = Wai.defaultRequest {
      Wai.pathInfo = rPath r
    , Wai.rawPathInfo = B.toByteString . H.encodePathSegments . rPath $ r
    , Wai.queryString = H.queryTextToQuery . rQuery $ r
    , Wai.rawQueryString = B.toByteString . H.renderQueryText True . rQuery $ r
    , Wai.requestHeaders = toHTTPHeaders . rHeaders $ r
    , Wai.requestBody = encodedBody
    , Wai.requestBodyLength = BS.length encodedBody
    }
  where encodedBody = T.encodeUtf8 . rBody $ r


toHTTPHeader :: RequestHeader -> H.Header
toHTTPHeader = bimap (Case.mk . T.encodeUtf8) T.encodeUtf8

toHTTPHeaders :: RequestHeaders -> H.RequestHeaders
toHTTPHeaders = map toHTTPHeader
