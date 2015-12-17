{-# LANGUAGE DataKinds, GADTs #-}

module Deriving.Nat (Nat(..), N(..)) where

data Nat = Zero | Succ Nat

data N n where
  Z :: N Zero
  S :: N n -> N (Succ n)
