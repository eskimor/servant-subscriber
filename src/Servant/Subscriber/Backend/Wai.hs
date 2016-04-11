
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
import           Data.IORef
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
  requestResource app req sendResponse = do
      waiReq <- toWaiRequest req
      app waiReq waiWriteResponse
      return ResponseReceived

waiSendResponse :: (Response -> ResponseReceived) -> Wai.Response -> Wai.ResponseReceived
waiSendResponse sendResponse = fmap fixResponse . sendResponse . fromWaiResponse
  where fixResponse = const Wai.ResponseReceived

toWaiRequest :: HttpRequest -> IO Wai.Request
toWaiRequest r = do
  waiBody <- mkWaiRequestBody encodedBody
  return Wai.defaultRequest {
      Wai.pathInfo = H.decodePathSegments . rawPath . httpPath $ r
    , Wai.rawPathInfo = rawPath r
    , Wai.queryString = H.queryTextToQuery . httpQuery $ r
    , Wai.rawQueryString = B.toByteString . H.renderQueryText True . httpQuery $ r
    , Wai.requestHeaders = toHTTPHeaders . httpHeaders $ r
    , Wai.requestBody = waiBody
    , Wai.requestBodyLength = Wai.KnownLength . fromIntegral . BS.length $ encodedBody
    }
  where
    rawPath (Path raw) = raw
    encodedBody = toByteString . fromEncoding . toEncoding . httpBody $ r

mkWaiRequestBody :: ByteString -> IO (IO ByteString)
mkWaiRequestBody b = do
  var <- newIORef bs
  return $ do
    readIORef var
    writeIORef var BS.empty


fromWaiResponse :: Wai.Response -> Response
fromWaiResponse (Wai.ResponseBuilder status headers builder)=
    sendResponse c Response {
      httpStatus = fromHTTPStatus status
    , httpHeaders = fromHTTPHeaders headers
    , httpBody = ResponseBody builder
    }
sendResponse _ = error "I am sorry - this 'Response' type is not yet implemented in servant-subscriber!"
