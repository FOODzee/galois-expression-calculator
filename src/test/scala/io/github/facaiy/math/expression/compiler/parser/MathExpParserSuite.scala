package io.github.facaiy.math.expression.compiler.parser

import org.scalatest.FunSpec

import io.github.facaiy.math.expression.compiler.MathExpCompiler

/**
 * Created by facai on 6/9/17.
 */
class MathExpParserSuite extends FunSpec {
  describe("For math expression") {
    it("parse string to tokens correctly") {
      val expression = "1 ** 2 + 2 * $data - power(2, F) / 4"

      val tokens = MathExpCompiler(expression)

      assert(tokens === Right(
        Operator2("-",
          Operator2("+", Operator2("**", Constant(1), Constant(2)),
                         Operator2("*", Constant(2), Variable("data"))),
          Operator2("/", OperatorN("power", List(Constant(2), Constant(0xF))), Constant(4)))))
    }
  }
}
