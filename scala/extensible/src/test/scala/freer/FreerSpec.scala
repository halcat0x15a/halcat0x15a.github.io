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
  }

}
