package io.github.facaiy.math.expression

import io.github.foodzee.math.galois.GaloisField
import org.scalatest.FunSpec

/**
 * Created by facai on 6/19/17.
 */
class MathExpSuite extends FunSpec {
  describe("MathExp") {
    implicit val field = new GaloisField(2, 4, 0x13)

    it("valid string") {
      val str = "1 + (2 * $a1) + $a2 ** 2"

      val ex = MathExp.parse(str)

      assert(ex.eval(Map("a1" -> 0, "a2" -> 0)) === 1)
      assert(ex.eval(Map("a1" -> 2, "a2" -> 1)) === 4)
      assert(ex.eval(Map("a1" -> 8, "a2" -> 2)) === 6)
    }

    it("invalid string") {
      val str = "1.0 + sqrt( - 2"

      assertThrows[IllegalArgumentException] {
        MathExp.parse(str)
      }
    }
  }
}
