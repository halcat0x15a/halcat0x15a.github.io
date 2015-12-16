package typelevel

trait Plus[N <: Nat, M <: Nat] {

  type Result <: Nat

}

object Plus {

  implicit def zero[N <: Nat]: Plus[N, Zero] { type Result = N } =
    new Plus[N, Zero] {
      type Result = N
    }

  implicit def succ[N <: Nat, M <: Nat](implicit plus: Plus[N, M]): Plus[N, Succ[M]] { type Result = Succ[plus.Result] } =
    new Plus[N, Succ[M]] {
      type Result = Succ[plus.Result]
    }

}
