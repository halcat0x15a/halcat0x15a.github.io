package extensible

import org.scalatest.FunSpec

class EffSpec extends FunSpec {

  describe("Eff") {
    it("is Tree") {
      type U = Tree :+: Void
      implicit val m = Member[Tree, U]
      val r = for {
        x <- node(leaf(0), node(leaf(1), leaf(2)))
        y <- node(leaf(x), leaf(x))
      } yield y + 1
      assert(Eff.run(str(r)) == "((1, 1), ((2, 2), (3, 3)))")
    }
    it("is Maybe") {
      type U = Maybe :+: Void
      implicit val m = Member[Maybe, U]
      val e1 = for {
        x <- just(2)
        y <- just(3)
      } yield x + y
      assert(Eff.run(maybe(e1)(-1)) == 5)
      val e2 = for {
        x <- just(2)
        y <- nothing[U, Int]
      } yield x + y
      assert(Eff.run(maybe(e2)(-1)) == -1)
    }
    it("is Tree and Maybe") {
      def e1[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Eff[U, Int] =
        for {
          x <- just(0)
          y <- just(1)
          z <- node(leaf(x), leaf(y))
        } yield z + 1
      assert(Eff.run(maybe(str(e1[Tree :+: Maybe :+: Void]))("fail")) == "(1, 2)")
      def e2[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Eff[U, Int] =
        for {
          x <- just(0)
          y <- nothing[U, Int]
          z <- node(leaf(x), leaf(y))
        } yield z + 1
      assert(Eff.run(maybe(str(e2[Tree :+: Maybe :+: Void]))("fail")) == "fail")
      assert(Eff.run(str(maybe(e2[Maybe :+: Tree :+: Void])(-1))) == "-1")
      assert(Eff.run(str(maybe(e1[Maybe :+: Tree :+: Void])(-1))) == "(1, 2)")
    }
  }

}
