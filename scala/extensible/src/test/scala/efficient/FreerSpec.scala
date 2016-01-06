package efficient

import org.scalatest.FunSpec

class EfficientFreerSpec extends FunSpec {

  describe("Freer") {
    it("is Tree") {
      val r = for {
        x <- node(leaf(0), node(leaf(1), leaf(2)))
        y <- node(leaf(x), leaf(x))
      } yield y + 1
      assert(str(r) == "((1, 1), ((2, 2), (3, 3)))")
    }
    it("is Maybe") {
      val e1 = for {
        x <- just(2)
        y <- just(3)
      } yield x + y
      assert(maybe(e1)(-1) == 5)
      val e2 = for {
        x <- just(2)
        y <- nothing[Int]
      } yield x + y
      assert(maybe(e2)(-1) == -1)
    }
  }

}
