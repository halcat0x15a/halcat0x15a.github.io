package typelevel

trait Minus[N <: Nat, M <: Nat] {

  type Result <: Nat

}

object Minus {

  implicit def zero[N <: Nat]: Minus[N, Zero] { type Result = N } =
    new Minus[N, Zero] {
      type Result = N
    }

  implicit def succ[N <: Nat, M <: Nat](implicit minus: Minus[N, M]): Minus[Succ[N], Succ[M]] { type Result = minus.Result } =
    new Minus[Succ[N], Succ[M]] {
      type Result = minus.Result
    }

}
