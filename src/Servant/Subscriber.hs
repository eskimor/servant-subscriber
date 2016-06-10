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
) where

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVar, modifyTVar')
import           Data.Aeson
import           GHC.Generics
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import Debug.Trace (trace)
import Network.URI (URI(..), pathSegments)
import Data.Proxy
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
import           Network.WebSockets.Connection as WS
import           Servant.Server
import Servant.Utils.Links (IsElem, HasLink, MkLink, safeLink)
import Control.Monad
import Servant.Subscriber.Subscribable
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import GHC.Conc
import Network.Wai
import Network.Wai.Handler.WebSockets
import Data.Monoid ((<>))

import Servant.Subscriber.Types
import qualified Servant.Subscriber.Client as Client
import Servant.Subscriber.Backend.Wai

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
          forkPingThread connection 25
          runLog . Client.run app subscriber <=< atomically . Client.fromWebSocket $ connection
    if Path (pathInfo req) == entryPoint subscriber
      then websocketsOr opts handleWSConnection app req sendResponse
      else app req sendResponse
