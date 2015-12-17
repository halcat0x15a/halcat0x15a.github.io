{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}

module Deriving.Lens (Lens(..), compose, GenericLens(..), lens) where

import GHC.Generics
import Deriving.Nat

data Lens a b = Lens { get :: a -> b, set :: b -> a -> a }

compose :: Lens b c -> Lens a b -> Lens a c
compose f g = Lens (get f . get g) (\c a -> set g (set f c (get g a)) a)

class GenericLens n f where
  type Nth n f
  genLens :: N n -> Lens (f a) (Nth n f)

instance GenericLens n (K1 i c) where
  type Nth n (K1 i c) = c
  genLens n = Lens unK1 $ \a _ -> K1 a

instance GenericLens n f => GenericLens n (M1 i t f) where
  type Nth n (M1 i t f) = Nth n f
  genLens n = compose (genLens n) (Lens unM1 $ \a _ -> M1 a)

instance GenericLens Zero f => GenericLens Zero (f :*: g) where
  type Nth Zero (f :*: g) = Nth Zero f
  genLens n = compose (genLens n) (Lens (\case a :*: _ -> a) (\a -> \case _ :*: b -> a :*: b))

instance GenericLens n g => GenericLens (Succ n) (f :*: g) where
  type Nth (Succ n) (f :*: g) = Nth n g
  genLens (S n) = compose (genLens n) (Lens (\case _ :*: b -> b) (\b -> \case a :*: _ -> a :*: b))

lens :: (Generic a, GenericLens n (Rep a)) => N n -> Lens a (Nth n (Rep a))
lens n = Lens (get (genLens n) . from) (\b -> to . set (genLens n) b . from)
