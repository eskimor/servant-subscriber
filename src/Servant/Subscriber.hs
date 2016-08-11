{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}


module Servant.Subscriber (
  notify
, makeSubscriber
, serveSubscriber
, Subscriber
, Event(..)
, IsElem
, HasLink
, IsValidEndpoint
, IsSubscribable
, Proxy
, MkLink
, URI
, STM
) where


import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map as Map
import           Data.Proxy
import           Network.URI (URI(..), pathSegments)
import           Network.Wai
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets.Connection as WS
import           Servant.Server
import           Servant.Subscriber.Subscribable
import           Servant.Utils.Links (IsElem, HasLink, MkLink, safeLink)

import           Servant.Subscriber.Backend.Wai ()
import qualified Servant.Subscriber.Client as Client
import           Servant.Subscriber.Types


makeSubscriber :: Path -> LogRunner -> STM (Subscriber api)
makeSubscriber entryPoint logRunner = do
  state <- newTVar Map.empty
  return $ Subscriber state entryPoint logRunner

serveSubscriber :: forall api. (HasServer api '[]) => Subscriber api -> Server api -> Application
serveSubscriber subscriber server req sendResponse = do
    let app = serve (Proxy :: Proxy api) server
    let opts = defaultConnectionOptions
    let runLog = runLogging subscriber
    let handleWSConnection pending = do
          connection <- acceptRequest pending
          forkPingThread connection 28
          runLog . Client.run app <=< atomically . Client.fromWebSocket subscriber $ connection
    if Path (pathInfo req) == entryPoint subscriber
      then websocketsOr opts handleWSConnection app req sendResponse
      else app req sendResponse
