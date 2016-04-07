{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}






module Servant.Subscriber.Subscribable where

import           Data.Proxy
import           GHC.Exts            (Constraint)
import           GHC.TypeLits
import           Servant
import           Servant.Utils.Links


data Subscribable (subscriptions :: [EventName])


-- | You may use this type family to tell the type checker that your custom
-- type may be skipped as part of a link. This is useful for things like
-- 'QueryParam' that are optional in a URI and are not part of a subscription uri.
--
-- >>> data CustomThing
-- >>> type instance IsSubscribable' e (CustomThing :> sa) s = IsSubscribable e sa s
--
-- Note that 'IsSubscribable' is called, which will mutually recurse back to `IsSubscribable'`
-- if it exhausts all other options again.
--
-- Once you have written a HasSubscription instance for CustomThing you are ready to
-- go.
type family IsSubscribable' endpoint api (subscription :: EventName) :: Constraint


type family IsSubscribable endpoint api (subscription :: EventName) :: Constraint where
    IsSubscribable e (sa :<|> sb) s             = Or (IsSubscribable e sa s) (IsSubscribable e sb s)
    IsSubscribable sa (Subscribable subscriptions :> sb) s
                                                = Elem s subscriptions `And` IsElem sa sb
    IsSubscribable (e :> sa) (e :> sb) s        = IsSubscribable sa sb s
    IsSubscribable sa (Header sym x :> sb) s    = IsSubscribable sa sb s
    IsSubscribable sa (ReqBody y x :> sb)  s    = IsSubscribable sa sb s
    IsSubscribable (Capture z y :> sa) (Capture x y :> sb) s
                                                = IsSubscribable sa sb s
    IsSubscribable sa (QueryParam x y :> sb) s  = IsSubscribable sa sb s
    IsSubscribable sa (QueryParams x y :> sb) s = IsSubscribable sa sb s
    IsSubscribable sa (QueryFlag x :> sb) s     = IsSubscribable sa sb s
    IsSubscribable e a s                        = IsSubscribable' e a s


-- | A valid endpoint may only contain Symbols and captures:
type family IsValidEndpoint endpoint :: Constraint where
  IsValidEndpoint ((sym :: Symbol) :> sub) = IsValidEndpoint sub
  IsValidEndpoint (Capture z y :> sub)     = IsValidEndpoint sub

instance HasServer sublayout context => HasServer (Subscribable s :> sublayout) context where
  type ServerT (Subscribable s :> sublayout) m = ServerT sublayout m
  route _ = route (Proxy :: Proxy sublayout)

-------------- Copied from Servant.Util.Links (they are not exported) ----------

-- | If both a or b produce an empty constraint, produce an empty constraint.
type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () ()     = ()

type family Elem e es :: Constraint where
    Elem x (x ': xs) = ()
    Elem y (x ': xs) = Elem y xs

--------------------------------------------------------------------------------


data EventName = CreatedEvent | ModifiedEvent | DeletedEvent deriving (Eq, Ord, Show)

class EventNameFromProxy (a :: EventName) where
  fromEventNameProxy :: Proxy a -> EventName

instance EventNameFromProxy 'CreatedEvent where
  fromEventNameProxy _ = CreatedEvent

instance EventNameFromProxy 'ModifiedEvent where
  fromEventNameProxy _ = ModifiedEvent

instance EventNameFromProxy 'DeletedEvent where
  fromEventNameProxy _ = DeletedEvent
