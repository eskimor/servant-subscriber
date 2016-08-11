{-# LANGUAGE OverloadedStrings #-}

import           Data.Proxy                         (Proxy (..))
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.Subscriber.Request
import           Servant.Subscriber.Response
import           Servant.Subscriber.Types


myTypes = [
       mkSumType (Proxy :: Proxy Request)
     , mkSumType (Proxy :: Proxy HttpRequest)
     , mkSumType (Proxy :: Proxy Response)
     , mkSumType (Proxy :: Proxy HttpResponse)
     , mkSumType (Proxy :: Proxy Status)
     , mkSumType (Proxy :: Proxy Path)
     ]

bridgeResponseBody :: BridgePart
bridgeResponseBody = do
  typeName ^== "ResponseBody"
  typeModule ^== "Servant.Subscriber.Response"
  return psString -- For now

bridgeRequestBody :: BridgePart
bridgeRequestBody = do
  typeName ^== "RequestBody"
  typeModule ^== "Servant.Subscriber.Request"
  return psString

myBridge :: BridgePart
myBridge = bridgeResponseBody <|> bridgeRequestBody <|> defaultBridge

main :: IO ()
main = writePSTypes "../purescript-subscriber/src" (buildBridge myBridge) myTypes
