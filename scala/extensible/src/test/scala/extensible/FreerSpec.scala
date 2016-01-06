package extensible

import org.scalatest.FunSpec

class ExtensibleFreerSpec extends FunSpec {

  describe("Freer") {
    it("is Tree") {
      type U = Tree :+: Void
      val r = for {
        x <- node[U, Int](leaf(0), node(leaf(1), leaf(2)))
        y <- node[U, Int](leaf(x), leaf(x))
      } yield y + 1
      assert(Freer.run(str(r)) == "((1, 1), ((2, 2), (3, 3)))")
    }
    it("is Maybe") {
      type U = Maybe :+: Void
      val e1 = for {
        x <- just[U, Int](2)
        y <- just[U, Int](3)
      } yield x + y
      assert(Freer.run(maybe(e1)(-1)) == 5)
      val e2 = for {
        x <- just[U, Int](2)
        y <- nothing[U, Int]
      } yield x + y
      assert(Freer.run(maybe(e2)(-1)) == -1)
    }
    it("is Tree and Maybe") {
      def e1[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Freer[U, Int] =
        for {
          x <- just[U, Int](0)
          y <- just[U, Int](1)
          z <- node[U, Int](leaf(x), leaf(y))
        } yield z + 1
      assert(Freer.run(maybe(str(e1[Tree :+: Maybe :+: Void]))("fail")) == "(1, 2)")
      def e2[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Freer[U, Int] =
        for {
          x <- just[U, Int](0)
          y <- nothing[U, Int]
          z <- node[U, Int](leaf(x), leaf(y))
        } yield z + 1
      assert(Freer.run(maybe(str(e2[Tree :+: Maybe :+: Void]))("fail")) == "fail")
      assert(Freer.run(str(maybe(e1[Maybe :+: Tree :+: Void])(-1))) == "(-1, -1)")
    }
  }

}
