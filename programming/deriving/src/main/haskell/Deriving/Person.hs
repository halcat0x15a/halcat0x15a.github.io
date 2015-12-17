{-# LANGUAGE DeriveGeneric #-}

module Deriving.Person (Person(..), personName, personAge) where

import GHC.Generics
import Deriving.Lens
import Deriving.Nat

data Person = Person { name :: String, age :: Int } deriving (Eq, Show, Generic)

personName :: Lens Person String
personName = lens Z

personAge :: Lens Person Int
personAge = lens (S Z)
