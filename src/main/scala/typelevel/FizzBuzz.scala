package typelevel

trait FizzBuzz[N <: Nat] {

  def apply(): String

}

trait FizzBuzzLowestPriorityImplicits {

  implicit def number[N <: Nat](implicit toInt: ToInt[N]): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = toInt().toString
    }

}

trait FizzBuzzLowerPriorityImplicits extends FizzBuzzLowestPriorityImplicits {

  implicit def fizz[N <: Nat](implicit mod: Mod[N, _3] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "Fizz"
    }

  implicit def buzz[N <: Nat](implicit mod: Mod[N, _5] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "Buzz"
    }

}

object FizzBuzz extends FizzBuzzLowerPriorityImplicits {

  def apply[N <: Nat](implicit lt: LT[_0, N], fizzbuzz: FizzBuzz[N]): String = fizzbuzz()

  implicit def fizzbuzz[N <: Nat](implicit mod: Mod[N, _15] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "FizzBuzz"
    }

}
