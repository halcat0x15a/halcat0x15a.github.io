package typelevel

trait ToInt[N <: Nat] {

  def apply(): Int

}

object ToInt {

  def apply[N <: Nat](implicit toInt: ToInt[N]): Int = toInt()

  implicit def zero: ToInt[Zero] =
    new ToInt[Zero] {
      def apply(): Int = 0
    }

  implicit def succ[N <: Nat](implicit toInt: ToInt[N]): ToInt[Succ[N]] =
    new ToInt[Succ[N]] {
      def apply(): Int = toInt() + 1
    }

}
