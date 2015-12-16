package deriving

import org.scalatest.FunSpec

class DerivingLensSpec extends FunSpec {

  describe("Deriving Lens in Java") {

    it("identity") {
      val age = java.Lens.lens[java.Person, Int](classOf, "age")
      assert(age.get(age.set(new java.Person("halcat0x15a", 21), 22)) == 22)
    }

  }

  describe("Deriving Lens in Scala") {

    it("identity") {
      val age = scala.Lens.lens[scala.Person, Int]("age")
      assert(age.get(age.set(new scala.Person("halcat0x15a", 21), 22)) == 22)
    }

  }

}
