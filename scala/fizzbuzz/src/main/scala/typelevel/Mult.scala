package typelevel

trait Mult[N <: Nat, M <: Nat] {

  type Result <: Nat

}

object Mult {

  implicit def zero[N <: Nat]: Mult[N, Zero] { type Result = Zero } =
    new Mult[N, Zero] {
      type Result = Zero
    }

  implicit def one[N <: Nat]: Mult[N, Succ[Zero]] { type Result = N } =
    new Mult[N, Succ[Zero]] {
      type Result = N
    }

  implicit def succ[N <: Nat, M <: Nat, R <: Nat](implicit mult: Mult[N, Succ[M]] { type Result = R }, plus: Plus[N, R]): Mult[N, Succ[Succ[M]]] { type Result = plus.Result } =
    new Mult[N, Succ[Succ[M]]] {
      type Result = plus.Result
    }

}
