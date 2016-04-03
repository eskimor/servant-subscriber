{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}


import Control.Monad.Except
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

       
import Lib


type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Generic, Show)

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Generic, Show)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server3 :: ServerT API (ExceptT ServantErr IO)
server3 = position
     :<|> hello
     :<|> marketing

  where position :: Int -> Int -> ExceptT ServantErr IO Position
        position x y = return (Position x y)

        hello :: Maybe String -> ExceptT ServantErr IO HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> ExceptT ServantErr IO Email
        marketing clientinfo = return (emailForClient clientinfo)

userAPI :: Proxy API
userAPI = Proxy

app3 :: Application
app3 = serve userAPI server3


callServer3 :: forall endpoint. (GetEndpoint API endpoint (PickLeftRight endpoint API))
               => Proxy endpoint
               -> ServerT endpoint (ExceptT ServantErr IO)
callServer3 pE = callHandler (Proxy :: Proxy API) server3 pE

main :: IO ()
main = do
  -- Let's call handler by url!
  let endpoint :: Proxy ("position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position)
      endpoint = Proxy
  pos <- runExceptT $ callServer3 endpoint 9 2
  print pos
  let endpoint2 :: Proxy ("hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage)
      endpoint2 = Proxy
  hello <- runExceptT $ callServer3 endpoint2 $ Just "Robert"
  hello2 <- runExceptT $ callServer3 endpoint2 Nothing
  print hello
  print hello2
