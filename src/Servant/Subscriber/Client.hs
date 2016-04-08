module Servant.Subscriber.Client where



-- | Any message from the server is a Response.
data Response = Response {
      responseResource  :: Url
    , responseEventName :: EventName
    -- | Response data - Nothing on DeletedEvent:
    , responseData      :: Maybe ResponseData
    }
  | ServerError Url EventName ServantErr
  | RequestError Request RequestError
  deriving Generic

data RequestError = ResourceNotAvailable | SubscriptionNotAllowed deriving (Show, Generic)




data Client =
  Client {
    -- | When did we receive our last pong - close connections after some timeout.
    lastPong      :: TVar UTCTime
    -- | Needed - if we want immediate cleanup on connection drop:
  , subscriptions :: TVar [ChangeEvent]
  , connection    :: WS.Connection
  }
