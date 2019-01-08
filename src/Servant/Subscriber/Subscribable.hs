{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}



module Servant.Subscriber.Subscribable (
    Subscribable
  , IsSubscribable
  , IsSubscribable'
  , IsElem
  , IsValidEndpoint
  ) where

import           Control.Lens
import           Data.Proxy
import           GHC.Exts                 (Constraint)
import           GHC.TypeLits

import           Servant
import           Servant.Foreign
import           Servant.Foreign.Internal (_FunctionName)


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
    IsSubscribable sa (Subscribable :> sb)   = ()
    IsSubscribable e (sa :<|> sb)            = Or (IsSubscribable e sa) (IsSubscribable e sb)
    IsSubscribable ((sym :: Symbol) :> sa) (sym :> sb)       = IsSubscribable sa sb
    IsSubscribable (e :> sa) (e :> sb)       = IsSubscribable sa sb
    IsSubscribable sa (Header sym x :> sb)   = IsSubscribable sa sb
    IsSubscribable sa (ReqBody y x :> sb)    = IsSubscribable sa sb
    IsSubscribable (Capture z y :> sa) (Capture x y :> sb)
                                             = IsSubscribable sa sb
    IsSubscribable sa (QueryParam x y :> sb) = IsSubscribable sa sb
    IsSubscribable sa (QueryParams x y :> sb)= IsSubscribable sa sb
    IsSubscribable sa (QueryFlag x :> sb)    = IsSubscribable sa sb
    IsSubscribable e a                       = IsSubscribable' e a


type instance IsElem' sa (Subscribable :> sb) = SubscribableIsElem sa (Subscribable :> sb)

type family SubscribableIsElem endpoint api :: Constraint where
  SubscribableIsElem (Subscribable :> e) (Subscribable :> s) = IsElem e s
  SubscribableIsElem e (Subscribable :> s) = IsElem e s

-- | A valid endpoint may only contain Symbols and captures & for convenince Subscribable:
type family IsValidEndpoint endpoint :: Constraint where
  IsValidEndpoint ((sym :: Symbol) :> sub) = IsValidEndpoint sub
  IsValidEndpoint (Capture z y :> sub)     = IsValidEndpoint sub
  IsValidEndpoint (Subscribable :> sub)    = IsValidEndpoint sub
  IsValidEndpoint (Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) (a :: *)) = ()

instance HasServer sublayout context => HasServer (Subscribable :> sublayout) context where
  type ServerT (Subscribable :> sublayout) m = ServerT sublayout m
  route _ = route (Proxy :: Proxy sublayout)


instance HasForeign lang ftype sublayout => HasForeign lang ftype (Subscribable :> sublayout) where
  type Foreign ftype (Subscribable :> sublayout) = Foreign ftype sublayout
  -- foreignFor :: Proxy lang -> Proxy ftype -> Proxy layout -> Req ftype -> Foreign ftype layout
  foreignFor lang ftype _ req = foreignFor lang ftype (Proxy :: Proxy sublayout) $
                                          req & reqFuncName . _FunctionName %~ ("" :) -- Prepend empty string for marking as subscribable.

instance HasLink sub => HasLink (Subscribable :> sub) where
    type MkLink (Subscribable :> sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy :: Proxy sub)
