package fpinscala.testing

/*
trait Prop { self =>

  def check: Boolean

  def &&(that: => Prop): Prop =
    new Prop {
      def check: Boolean = self.check && that.check
    }

}
 */

object Prop {
  //val intList = Gen.liftOf(Gen.choose(0, 100))
  //val sumProp = forAll(intList)(ns => ns.sum == ns.reverse.sum) && forAll(intTuple) { case (n, m) => List.fill(n)(m).sum == m * n }
  //val maxProp = forAll(intList)(ns => ns.max == ns.reverse.max) && forAll(intTuple) { case (n, m) => List.fill(n)(m).max == m }
}
