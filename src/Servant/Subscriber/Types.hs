{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.Subscriber.Types where

import           Data.Aeson.Types
import           Data.Text        (Text)
import           GHC.Generics

newtype Path = Path [Text] deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

toSegments :: Path -> [Text]
toSegments (Path xs) = xs
