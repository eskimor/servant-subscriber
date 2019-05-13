{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Servant.Subscriber.Response where


import qualified Blaze.ByteString.Builder       as B
import qualified Blaze.ByteString.Builder.Char8 as B
import           Data.Aeson
import           Data.Aeson.Parser              (value)
import           Data.Aeson.Types               (unsafeToEncoding)
import           Data.Attoparsec.ByteString     (parseOnly)
import           Data.Bifunctor
import qualified Data.ByteString.Lazy         as BS
import qualified Data.CaseInsensitive           as Case
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           GHC.Generics
import qualified Network.HTTP.Types             as H
import           Servant.Server

import qualified Servant.Subscriber.Request      as R
import           Servant.Subscriber.Types


type ResponseHeader = R.RequestHeader
type ResponseHeaders = R.RequestHeaders

-- | Any message from the server is a Response.
--
--   'Subscribed': Resource was successfully subscribed
--
--   'Modified': Resource was modified (this message is also triggered immediately after a subscription)
--
--   'HttpRequestFailed': The server replied with some none 2xx status code.
--   Thus your subscription failed or got removed.
--
--   'ParseError': Your request could not be parsed.

data Response =
    Subscribed !R.HttpRequest
  | Modified !R.HttpRequest !ResponseBody -- If the full response is needed an additional FullSubscribe command with an appropriate additional response type will need to be added.
  | Deleted !Path
  | Unsubscribed !R.HttpRequest
  | HttpRequestFailed !R.HttpRequest !HttpResponse
  | ParseError
  deriving Generic

instance ToJSON Response

data HttpResponse = HttpResponse {
  httpStatus  :: !Status
, httpHeaders :: !ResponseHeaders
, httpBody    :: !ResponseBody
} deriving Generic

instance ToJSON HttpResponse

data Status = Status {
  statusCode    :: !Int
, statusMessage :: !Text
} deriving (Generic, Show)

instance ToJSON Status

type ResponseBody = Text


fromHTTPHeader :: H.Header -> ResponseHeader
fromHTTPHeader = bimap (T.decodeUtf8 . Case.original) T.decodeUtf8

fromHTTPHeaders :: H.ResponseHeaders -> ResponseHeaders
fromHTTPHeaders = map fromHTTPHeader

fromHTTPStatus :: H.Status -> Status
fromHTTPStatus s = Status {
  statusCode = H.statusCode s
, statusMessage = T.decodeUtf8 . H.statusMessage $ s
}

#if MIN_VERSION_servant(0,16,0)
fromServantError :: ServerError -> HttpResponse
#else
fromServantError :: ServantErr -> HttpResponse
#endif
fromServantError err =  HttpResponse {
  httpStatus = Status (errHTTPCode err) (T.pack $ errReasonPhrase err)
, httpHeaders = fromHTTPHeaders . errHeaders $ err
, httpBody = T.decodeUtf8 . BS.toStrict . errBody $ err
}

wrapInString :: B.Builder -> B.Builder
wrapInString x = B.fromChar '"' <> x <>  B.fromChar '"'
