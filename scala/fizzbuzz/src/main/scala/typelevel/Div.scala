package typelevel

trait Div[N <: Nat, M <: Nat] {
  type Result <: Nat
}

object Div {
  implicit def zero[N <: Nat, M <: Nat](implicit lt: LT[N, M]): Div[N, M] { type Result = Zero } =
    new Div[N, M] {
      type Result = Zero
    }

  implicit def succ[N <: Nat, M <: Nat, R <: Nat](implicit minus: Minus[N, M] { type Result = R }, div: Div[R, M]): Div[N, M] { type Result = Succ[div.Result] } =
    new Div[N, M] {
      type Result = Succ[div.Result]
    }
}
