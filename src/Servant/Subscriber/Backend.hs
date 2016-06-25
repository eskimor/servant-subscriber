-- | A backend for subscriber allows us to query a server and get a response.
-- | We mimick Wai's interface but with 'Request' and 'Response' types that match our needs.
-- | Our only backend right now actually is implemented via the WAI Application interface, see: "Servant.Subscriber.Backend.Wai".
module Servant.Subscriber.Backend where

import Servant.Subscriber.Request
import Servant.Subscriber.Response

data ResponseReceived = ResponseReceived

type Application = HttpRequest -> (HttpResponse -> IO ResponseReceived) -> IO ResponseReceived

class Backend a where
  requestResource :: a -> Application
