package typelevel

import org.scalatest.FunSpec

class FizzBuzzSpec extends FunSpec {

  describe("FizzBuzz") {

    it("should return Fizz or Buzz or FizzBuzz or numbers") {
      assert(FizzBuzz[_1] == "1")
      assert(FizzBuzz[_2] == "2")
      assert(FizzBuzz[_3] == "Fizz")
      assert(FizzBuzz[_4] == "4")
      assert(FizzBuzz[_5] == "Buzz")
      assert(FizzBuzz[_6] == "Fizz")
      assert(FizzBuzz[_7] == "7")
      assert(FizzBuzz[_8] == "8")
      assert(FizzBuzz[_9] == "Fizz")
      assert(FizzBuzz[_10] == "Buzz")
      assert(FizzBuzz[_11] == "11")
      assert(FizzBuzz[_12] == "Fizz")
      assert(FizzBuzz[_13] == "13")
      assert(FizzBuzz[_14] == "14")
      assert(FizzBuzz[_15] == "FizzBuzz")
    }

  }
}
