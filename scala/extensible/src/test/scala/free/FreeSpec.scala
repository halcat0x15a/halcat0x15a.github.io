package free

import org.scalatest.FunSpec

class FreeSpec extends FunSpec {

  describe("Free") {
    it("is Tree") {
      val r = for {
        x <- node(leaf(0), node(leaf(1), leaf(2)))
        y <- node(leaf(x), leaf(x))
      } yield y + 1
      assert(r == node(node(leaf(1), leaf(1)), node(node(leaf(2), leaf(2)), node(leaf(3), leaf(3)))))
    }
  }

}
