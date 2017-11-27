package io.github.facaiy.math.expression

import org.scalatest.FunSpec

/**
 * Created by facai on 6/19/17.
 */
class MathExpSuite extends FunSpec {
  describe("MathExp") {
    it("valid string") {
      val str = "1 + (2 * $a1) + $a2 ** 2"

      val ex = MathExp.parse(str)

      assert(ex.eval(Map("a1" -> 0, "a2" -> 0)) === 1)
      assert(ex.eval(Map("a1" -> 2, "a2" -> 1)) === 6)
      assert(ex.eval(Map("a1" -> 8, "a2" -> 2)) === 21)
    }

    it("invalid string") {
      val str = "1.0 + sqrt( - 2"

      assertThrows[IllegalArgumentException] {
        MathExp.parse(str)
      }
    }
  }
}
