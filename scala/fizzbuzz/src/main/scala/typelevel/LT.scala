package typelevel

trait LT[N <: Nat, M <: Nat]

object LT {

  implicit def zero[N <: Nat]: LT[Zero, Succ[N]] = new LT[Zero, Succ[N]] {}

  implicit def succ[N <: Nat, M <: Nat](implicit lt: LT[N, M]): LT[Succ[N], Succ[M]] = new LT[Succ[N], Succ[M]] {}

}
