package io.github.facaiy.math.expression

import io.github.facaiy.math.expression.compiler.parser._
import io.github.foodzee.math.Field

/**
 * Created by facai on 6/19/17.
 */
case class Expression[A, B](eval: A => B) {
  def map2[C, D](that: Expression[A, C])(g: (B, C) => D): Expression[A, D] =
    Expression(x => g(this.eval(x), that.eval(x)))

  def map[C](g: B => C): Expression[A, C] = Expression(g compose eval)
}

object Expression {
  import FunctionRegister._

  def toExpression[E](ast: MathExpAST)(implicit f: Field[E]): Expression[String => E, E] = ast match {
    case Constant(d: E) => Expression(_ => d)
    case c @ Constant(_) => throw new IllegalArgumentException(c.toString)
    case Variable(n) => Expression(f => f(n))
    case Operator2(op, v1, v2) =>
      toExpression(v1)(f).map2(toExpression(v2))(function2(f)(op))
    case opn @ OperatorN(op, as) =>
      val args = sequence(as.map(toExpression(_)(f))).map(_.toSeq)
      args.map{ xs: Seq[E] =>
        xs.length match {
          case 1 => function1(f)(op)(xs.head)
          case 2 => function2(f)(op)(xs.head, xs(2))
          case _ => throw new UnsupportedOperationException(opn.toString)
        }
      }
  }

  def unit[A, B](b: => B): Expression[A, B] = Expression(_ => b)

  def sequence[A, B](ls: List[Expression[A, B]]): Expression[A, List[B]] =
    ls.foldRight(unit[A, List[B]](List.empty))(
                 (e, acc) => Expression(x => e.eval(x) :: acc.eval(x)))
}

object FunctionRegister {
  def function1[E](f: Field[E]): Map[String, E => E] = Map(
    "inv" -> f.inv,
    "-" -> f.neg
  )

  def function2[E](f: Field[E]): Map[String, (E, E) => E] = Map(
    "+" -> f.add,
    "-" -> f.sub,
    "*" -> f.mul,
    "/" -> f.div,
    "**" -> ((a,b) => f.pow(a, f.asInt(b)))
  )
}
