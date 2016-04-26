{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Servant.Subscriber.Subscribable where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Proxy
import           GHC.Exts            (Constraint)
import           GHC.Generics
import           GHC.TypeLits
import           Servant
import           Servant.Utils.Links


data Subscribable


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
type family IsSubscribable' endpoint api :: Constraint


type family IsSubscribable endpoint api :: Constraint where
    IsSubscribable e (sa :<|> sb)            = Or (IsSubscribable e sa) (IsSubscribable e sb)
    IsSubscribable sa (Subscribable :> sb)
                                             = IsElem sa sb
    IsSubscribable (e :> sa) (e :> sb)       = IsSubscribable sa sb
    IsSubscribable sa (Header sym x :> sb)   = IsSubscribable sa sb
    IsSubscribable sa (ReqBody y x :> sb)    = IsSubscribable sa sb
    IsSubscribable (Capture z y :> sa) (Capture x y :> sb)
                                             = IsSubscribable sa sb
    IsSubscribable sa (QueryParam x y :> sb) = IsSubscribable sa sb
    IsSubscribable sa (QueryParams x y :> sb)= IsSubscribable sa sb
    IsSubscribable sa (QueryFlag x :> sb)    = IsSubscribable sa sb
    IsSubscribable e a                       = IsSubscribable' e a


-- | A valid endpoint may only contain Symbols and captures:
type family IsValidEndpoint endpoint :: Constraint where
  IsValidEndpoint ((sym :: Symbol) :> sub) = IsValidEndpoint sub
  IsValidEndpoint (Capture z y :> sub)     = IsValidEndpoint sub

instance HasServer sublayout context => HasServer (Subscribable :> sublayout) context where
  type ServerT (Subscribable :> sublayout) m = ServerT sublayout m
  route _ = route (Proxy :: Proxy sublayout)

-------------- Copied from Servant.Util.Links (they are not exported) ----------

-- | If both a or b produce an empty constraint, produce an empty constraint.
type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () ()     = ()

type family Elem e es :: Constraint where
    Elem x (x ': xs) = ()
    Elem y (x ': xs) = Elem y xs

--------------------------------------------------------------------------------
