{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Lib where


import GHC.TypeLits
import Servant.API hiding (IsElem)
import Servant.Utils.Links hiding (IsElem, Or)
import Data.Proxy
import Servant.Server
import           GHC.Exts              (Constraint)
import Network.Wai (Application)
import Control.Monad.Trans.Except (ExceptT)



       
-- | Select a handler from an API by specifying a type level link.
-- | The value of this function is the selected handler which can be called.
callHandler :: forall api endpoint. (GetEndpoint api endpoint (PickLeftRight endpoint api))
            => Proxy api
            -> ServerT api (ExceptT ServantErr IO)
            -> Proxy endpoint
            -> ServerT endpoint (ExceptT ServantErr IO)
callHandler pA handlers pE = getEndpoint (Proxy :: Proxy (PickLeftRight endpoint api)) pM pA pE handlers
  where
    pM = Proxy :: Proxy (ExceptT ServantErr IO)

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or 'False 'True = 'True
  Or 'True 'False = 'True
  Or 'True 'True = 'True

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'False 'False = 'False
  And 'False 'True = 'False
  And 'True 'False = 'False
  And 'True 'True = 'True

type family Not (a :: Bool) :: Bool where
  Not 'False = 'True
  Not 'True = 'False

type family IsElem endpoint api :: Bool where
    IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)              = IsElem sa sb
    IsElem sa (Header sym x :> sb)          = IsElem sa sb
    IsElem sa (ReqBody y x :> sb)           = IsElem sa sb
    IsElem (Capture z y :> sa) (Capture x y :> sb)
                                            = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)        = IsElem sa sb
    IsElem sa (QueryParams x y :> sb)       = IsElem sa sb
    IsElem sa (QueryFlag x :> sb)           = IsElem sa sb
    IsElem (Verb m s ct typ) (Verb m s ct' typ)
                                            = IsSubList ct ct'
    IsElem e e                              = True
    IsElem sa sb                            = False

type family IsSubList a b :: Bool where
    IsSubList '[] b          = True
    IsSubList (x ': xs) y    = Elem x y `And` IsSubList xs y

type family Elem e es :: Bool where
    Elem x (x ': xs) = True
    Elem y (x ': xs) = Elem y xs
    Elem y '[] = False

type family EnableConstraint (c :: Constraint) (enable :: Bool)  :: Constraint where
     EnableConstraint c 'True = c
     EnableConstraint c 'False = ()


type family PickLeftRight endpoint api :: Bool where
  PickLeftRight endpoint (sa :<|> sb) = IsElem endpoint sb
  PickLeftRight endpoint sa = 'True



class GetEndpoint api endpoint (chooseRight :: Bool)  where
  getEndpoint :: forall m. Proxy chooseRight -> Proxy m -> Proxy api -> Proxy endpoint -> ServerT api m -> ServerT endpoint m

-- Left choice
instance (GetEndpoint b1 endpoint (PickLeftRight endpoint b1))  => GetEndpoint (b1 :<|> b2) endpoint 'False where
  getEndpoint _ pM _ pEndpoint (lS :<|> _) = getEndpoint pLeftRight pM (Proxy :: Proxy b1) pEndpoint lS
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint b1)

-- Right choice
instance (GetEndpoint b2 endpoint (PickLeftRight endpoint b2))  => GetEndpoint (b1 :<|> b2) endpoint 'True where
  getEndpoint _ pM _ pEndpoint (_ :<|> lR) = getEndpoint pLeftRight pM (Proxy :: Proxy b2) pEndpoint lR
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint b2)

-- Pathpiece
instance (KnownSymbol sym, GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (sym :> sa) (sym :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) server
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)

-- Capture
instance (GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (Capture sym a :> sa) (Capture sym1 a :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server a = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server a)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)

-- QueryParam
instance (GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (QueryParam sym a :> sa) (QueryParam sym a :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server ma = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server ma)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)

-- QueryParams
instance (KnownSymbol sym, GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (QueryParams sym a :> sa) (QueryParams sym a :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server as = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server as)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)

-- QueryFlag
instance (KnownSymbol sym, GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (QueryFlag sym :> sa) (QueryFlag sym :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server f = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server f)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)


-- Header
instance (KnownSymbol sym, GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (Header sym a :> sa) (Header sym a :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server ma = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server ma)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)


-- ReqBody
instance (GetEndpoint sa endpoint (PickLeftRight endpoint sa)) => GetEndpoint (ReqBody ct a :> sa) (ReqBody ct a :> endpoint) 'True where
  getEndpoint _ pM _ pEndpoint server a = getEndpoint pLeftRight pM (Proxy :: Proxy sa) (Proxy :: Proxy endpoint) (server a)
    where pLeftRight = Proxy :: Proxy (PickLeftRight endpoint sa)


-- Verb
  -- (method :: k1) (statusCode :: Nat) (contentTypes :: [*])
instance GetEndpoint (Verb (n :: StdMethod) (s :: Nat) (ct :: [*]) a) (Verb (n :: StdMethod) (s :: Nat) (ct :: [*]) a) 'True where
  getEndpoint _ _ _ _ server = server
