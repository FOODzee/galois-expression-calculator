package io.github.facaiy.math.expression

import io.github.facaiy.math.expression.compiler.MathExpCompiler
import io.github.foodzee.math.Field

/**
 * Created by facai on 6/19/17.
 */
sealed trait MathExpError
case class MathExpScannerError(msg: String) extends MathExpError
case class MathExpParserError(msg: String) extends MathExpError

object MathExp {
  def parse[E](s: String)(implicit f: Field[E]): Expression[String => E, E] =
    MathExpCompiler(s) match {
      case Right(ts) => Expression.toExpression(ts)
      case Left(e) => throw new IllegalArgumentException(e.toString)
    }
}
