package curryhoward

import org.scalatest.FunSpec

class CurryHowardSpec extends FunSpec {

  describe("Union types") {

    it("double") {
      assert(double(2) == "4")
      assert(double("2") == "22")
    }

  }

  describe("Forall") {

    it("mapping") {
      assert(mapping(opt2list)((Some("hoge"), None)) == (List("hoge"), Nil))
      assert(mapping(list2opt)((List(0, 1, 2), Nil)) == (Some(0), None))
    }

  }

}
