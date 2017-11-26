package freer

import org.scalatest.FunSpec

class FreerSpec extends FunSpec {

  describe("Freer") {
    it("is Tree") {
      val r = for {
        x <- node(leaf(0), node(leaf(1), leaf(2)))
        y <- node(leaf(x), leaf(x))
      } yield y + 1
      assert(str(r) == "((1, 1), ((2, 2), (3, 3)))")
    }

    it("is Maybe") {
      def safeDiv(n: Int, d: Int): Maybe[Int] = if (d == 0) none else some(n / d)
      val r = for {
        n <- safeDiv(4, 2)
        m <- safeDiv(n, 0)
      } yield m
      assert(maybe(r)(42) == 42)
    }
  }

}
