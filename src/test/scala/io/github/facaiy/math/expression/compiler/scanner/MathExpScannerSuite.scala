package io.github.facaiy.math.expression.compiler.scanner

import org.scalatest.FunSpec

import io.github.facaiy.math.expression.MathExpScannerError

/**
 * Created by facai on 6/8/17.
 */
class MathExpScannerSuite extends FunSpec {
  describe("For tokens") {
    import io.github.facaiy.math.expression.compiler.scanner.MathExpScanner._

    def parse(token: Parser[MathExpToken])
             (expression: String): Either[MathExpScannerError, MathExpToken] = {
      MathExpScanner.parse(token, expression) match {
        case NoSuccess(msg, next) => Left(MathExpScannerError(msg))
        case Success(result, next) => Right(result)
      }
    }

    it("number") {
      val p = parse(MathExpScanner.number) _

      // Int
      assert(p("1") === Right(NUMBER(1)))
      assert(p("10") === Right(NUMBER(1)))
      assert(p("E") === Right(NUMBER(0xe)))
      assert(p("d") === Right(NUMBER(0xD)))
      assert(p("1f") === Right(NUMBER(1)))

      // Float: parses until dot
      assert(p(".1").isLeft)
      assert(p("1.") === Right(NUMBER(1)))
      assert(p("1.0") === Right(NUMBER(1)))
      assert(p("-1.0") === Right(NUMBER(-1)))
      assert(p("1.02") === Right(NUMBER(1)))

      // Long: parses until L
      assert(p("1L") === Right(NUMBER(1)))

      // Invaild
      assert(p("+1.0").isLeft)
    }

    it("variable") {
      val p = parse(MathExpScanner.variable) _

      assert(p("$hello") === Right(VAR_NAME("hello")))
      assert(p("hello").isLeft)
      assert(p("$1hello").isLeft)
    }
  }

  describe("For math expression") {
    it("parse string to tokens correctly") {
      val expression = "1 + 2 * $data - power(2, a) / 4"
      assert(
        MathExpScanner(expression) ===
        Right(
          List(NUMBER(1), ADD, NUMBER(2), MULTIPLY, VAR_NAME("data"), MINUS,
               FUNC_NAME("power"), LEFT_PARENTHESIS, NUMBER(2), COMMA, NUMBER(0xa), RIGHT_PARENTHESIS,
               DIVIDE, NUMBER(4))))
    }
  }
}
