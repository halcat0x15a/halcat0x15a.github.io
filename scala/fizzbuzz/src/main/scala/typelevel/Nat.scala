package typelevel

trait Nat

trait Zero extends Nat

trait Succ[N <: Nat] extends Nat
