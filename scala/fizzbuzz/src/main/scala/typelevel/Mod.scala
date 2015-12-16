package typelevel

trait Mod[N <: Nat, M <: Nat] {

  type Result <: Nat

}

object Mod {

  implicit def mod[N <: Nat, M <: Nat, Q <: Nat, R <: Nat](implicit div: Div[N, M] { type Result = Q }, mult: Mult[Q, M] { type Result = R }, minus: Minus[N, R]): Mod[N, M] { type Result = minus.Result } =
    new Mod[N, M] {
      type Result = minus.Result
    }

}
