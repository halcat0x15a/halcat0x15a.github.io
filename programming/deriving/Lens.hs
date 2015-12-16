{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}

import GHC.Generics

data Lens a b = Lens {
  get :: a -> b,
  set :: a -> b -> a
}

compose :: Lens b c -> Lens a b -> Lens a c
compose f g = Lens (get f . get g) (\a -> (set g a) . (set f (get g a)))

data Person = Person { name :: String, age :: Int } deriving (Eq, Show, Generic)

data Nat = Zero | Succ Nat

data N n where
  Z :: N Zero
  S :: N n -> N (Succ n)

class GenericLens n f where
  type Nth n f
  genLens :: N n -> Lens (f a) (Nth n f)

instance GenericLens n (K1 i c) where
  type Nth n (K1 i c) = c
  genLens n = Lens unK1 (const K1)

instance GenericLens n f => GenericLens n (M1 i t f) where
  type Nth n (M1 i t f) = Nth n f
  genLens n = compose (genLens n) (Lens unM1 (const M1))

instance GenericLens Zero f => GenericLens Zero (f :*: g) where
  type Nth Zero (f :*: g) = Nth Zero f
  genLens n = compose (genLens n) (Lens (\case a :*: _ -> a) (\case _ :*: b -> \a -> a :*: b))

instance GenericLens n g => GenericLens (Succ n) (f :*: g) where
  type Nth (Succ n) (f :*: g) = Nth n g
  genLens (S n) = compose (genLens n) (Lens (\case _ :*: b -> b) (\case a :*: _ -> \b -> a :*: b))

lens :: (Generic a, GenericLens n (Rep a)) => N n -> Lens a (Nth n (Rep a))
lens n = Lens (get (genLens n) . from) (\a -> to . (set (genLens n) (from a)))

personName :: Lens Person String
personName = lens Z

personAge :: Lens Person Int
personAge = lens (S Z)
