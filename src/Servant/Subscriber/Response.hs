{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Response where


import qualified Blaze.ByteString.Builder        as B
import qualified Blaze.ByteString.Builder.Char8  as B
import           Data.Aeson
import           Data.Aeson.Parser               (value)
import           Data.Aeson.Types                (unsafeToEncoding)
import           Data.Attoparsec.ByteString      (parseOnly)
import           Data.Bifunctor
import qualified Data.CaseInsensitive            as Case
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           GHC.Generics
import qualified Network.HTTP.Types              as H
import           Servant.Server

import qualified Servant.Subscriber.Request      as R
import           Servant.Subscriber.Types


type ResponseHeader = R.RequestHeader
type ResponseHeaders = R.RequestHeaders

-- | Any message from the server is a Response.
data Response =
    Subscribed !Path -- |< Resource was successfully subscribed
  | Modified !Path !HttpResponse -- |< Can also be a non 2xx code, ServerError only triggers on the very first response, resulting in a failed subscription.
  | Deleted !Path
  | Unsubscribed !Path
  | RequestError !RequestError
  deriving Generic

instance ToJSON Response

data HttpResponse = HttpResponse {
  httpStatus  :: !Status
, httpHeaders :: !ResponseHeaders
, httpBody    :: ResponseBody
} deriving (Generic)

instance ToJSON HttpResponse

data Status = Status {
  statusCode    :: !Int
, statusMessage :: !Text
} deriving Generic

instance ToJSON Status

-- | Your subscription did not work out because:
data RequestError =
    ParseError
  | HttpRequestFailed !R.HttpRequest !HttpResponse -- |< The server replied with some none 2xx status code. Thus your subscription failed.
  | NoSuchSubscription !Path
  | AlreadySubscribed !Path deriving Generic

instance ToJSON RequestError


data ResponseBody = ResponseBody B.Builder deriving Generic

instance ToJSON ResponseBody where
  toJSON (ResponseBody b) = getValue $ parseOnly value (B.toByteString (wrapInString b))
    where
      getValue r = case r of
        Left e -> error e
        Right r -> r
  toEncoding (ResponseBody b) = unsafeToEncoding . wrapInString $ b

fromHTTPHeader :: H.Header -> ResponseHeader
fromHTTPHeader = bimap (T.decodeUtf8 . Case.original) T.decodeUtf8

fromHTTPHeaders :: H.ResponseHeaders -> ResponseHeaders
fromHTTPHeaders = map fromHTTPHeader

fromHTTPStatus :: H.Status -> Status
fromHTTPStatus s = Status {
  statusCode = H.statusCode s
, statusMessage = T.decodeUtf8 . H.statusMessage $ s
}

fromServantError :: ServantErr -> HttpResponse
fromServantError err =  HttpResponse {
  httpStatus = Status (errHTTPCode err) (T.pack $ errReasonPhrase err)
, httpHeaders = fromHTTPHeaders . errHeaders $ err
, httpBody = ResponseBody . B.fromLazyByteString . errBody $ err
}

wrapInString :: B.Builder -> B.Builder
wrapInString x = B.fromChar '"' <> x <>  B.fromChar '"'
