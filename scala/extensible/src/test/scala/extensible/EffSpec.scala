package extensible

import org.scalatest.FunSpec

class EffSpec extends FunSpec {
  describe("Eff") {
    it("is Writer") {
      def e1[R[_]](implicit w: Member[Writer, R]) = for {
        _ <- tell("hello, ")
        _ <- tell("world.")
      } yield 0
      assert(Eff.run(Writer.run(e1)) == ("hello, world.", 0))
    }

    it("is Maybe") {
      def e2[R[_]](implicit m: Member[Maybe, R]) = for {
        x <- some(2)
        y <- none[R, Int]
      } yield x + y
      assert(Eff.run(Maybe.run(-1)(e2)) == -1)
    }

    it("is Writer and Maybe") {
      def e3[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
        for {
          _ <- tell("hello, ")
          _ <- none[R, Unit]
          _ <- tell("world.")
        } yield 0
      assert(Eff.run(Writer.run(Maybe.run(-1)(e3))) == ("hello, ", -1))
      assert(Eff.run(Maybe.run(("fail", -1))(Writer.run(e3))) == ("fail", -1))
    }
  }
}
