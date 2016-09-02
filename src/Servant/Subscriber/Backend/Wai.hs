{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Backend of accessing a wai server:
module Servant.Subscriber.Backend.Wai where

import qualified Blaze.ByteString.Builder as B
import           Data.Aeson
import qualified Data.ByteString          as BS
import           Data.IORef
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Network.HTTP.Types       as H
import qualified Network.Wai              as Wai
import qualified Network.Wai.Internal     as Wai

import           Servant.Subscriber.Backend
import           Servant.Subscriber.Request as Req
import           Servant.Subscriber.Response as Res
import           Servant.Subscriber.Types



instance Backend Wai.Application where
  requestResource app req sendResponse = do
      waiReq <- toWaiRequest req
      app waiReq (waiSendResponse sendResponse)
      return ResponseReceived

waiSendResponse :: (HttpResponse -> IO ResponseReceived) -> Wai.Response -> IO Wai.ResponseReceived
waiSendResponse sendResponse = fmap fixResponse . sendResponse . fromWaiResponse
  where fixResponse = const Wai.ResponseReceived

toWaiRequest :: HttpRequest -> IO Wai.Request
toWaiRequest r = do
  waiBody <- mkWaiRequestBody encodedBody
  return Wai.defaultRequest {
      Wai.requestMethod = T.encodeUtf8 . httpMethod $ r
    , Wai.pathInfo = toSegments . httpPath $ r
    , Wai.rawPathInfo = B.toByteString . H.encodePathSegments . toSegments . httpPath $ r
    , Wai.queryString = H.queryTextToQuery . httpQuery $ r
    , Wai.rawQueryString = B.toByteString . H.renderQueryText True . httpQuery $ r
    , Wai.requestHeaders = toHTTPHeaders . (<> standardHeaders) . Req.httpHeaders $ r
    , Wai.requestBody = waiBody
    , Wai.requestBodyLength = Wai.KnownLength . fromIntegral . BS.length $ encodedBody
    }
  where
    encodedBody = T.encodeUtf8 . runRequestBody . Req.httpBody $ r

mkWaiRequestBody :: BS.ByteString -> IO (IO BS.ByteString)
mkWaiRequestBody b = do
  var <- newIORef b
  return $ do
    val <- readIORef var
    writeIORef var BS.empty
    return val


fromWaiResponse :: Wai.Response -> HttpResponse
fromWaiResponse (Wai.ResponseBuilder status headers builder)= HttpResponse {
      httpStatus      = fromHTTPStatus status
    , Res.httpHeaders = fromHTTPHeaders headers
    , Res.httpBody    = T.decodeUtf8 . B.toByteString $ builder
    }
fromWaiResponse _ = error "I am sorry - this 'Response' type is not yet implemented in servant-subscriber!"

standardHeaders :: RequestHeaders
standardHeaders = [
    ("Accept", "application/json")
  , ("Content-Type", "application/json")
  ]

lengthHeader :: BS.ByteString -> RequestHeader
lengthHeader body = ( "Content-Length", T.pack . show . BS.length $ body  )
