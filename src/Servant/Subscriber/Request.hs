{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.Subscriber.Request where



import           Data.Aeson
import           Data.Bifunctor
import qualified Data.CaseInsensitive as Case
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           GHC.Generics
import qualified Network.HTTP.Types as H
import           Servant.Subscriber.Types




{--| We don't use Network.HTTP.Types here, because we need FromJSON instances, which can not
     be derived for 'ByteString'
--}
type RequestHeader = (Text, Text)
type RequestHeaders = [RequestHeader]

-- | Any message from the client is a 'Request':
data Request =
    Subscribe !HttpRequest
  | Unsubscribe !HttpRequest
  deriving Generic

instance FromJSON Request
instance ToJSON Request


data HttpRequest = HttpRequest {
  httpMethod  :: !Text
, httpPath    :: !Path
, httpHeaders :: RequestHeaders
, httpQuery   :: H.QueryText
, httpBody    :: RequestBody
} deriving ( Generic, Eq, Ord )

instance FromJSON HttpRequest
instance ToJSON HttpRequest

newtype RequestBody = RequestBody Text deriving (Generic, ToJSON, FromJSON, Eq, Ord)

toHTTPHeader :: RequestHeader -> H.Header
toHTTPHeader = bimap (Case.mk . T.encodeUtf8) T.encodeUtf8

toHTTPHeaders :: RequestHeaders -> H.RequestHeaders
toHTTPHeaders = map toHTTPHeader

requestPath :: Request -> Path
requestPath req = httpPath $ case req of
                    Subscribe hReq -> hReq
                    Unsubscribe hReq -> hReq
