package io.github.facaiy.math.expression

import io.github.facaiy.math.expression.compiler.parser._

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

  def toExpression(ast: MathExpAST): Expression[String => Int, Int] = ast match {
    case Constant(d: Int) => Expression(_ => d)
    case c @ Constant(_) => throw new IllegalArgumentException(c.toString)
    case Variable(n) => Expression(f => f(n))
    case Operator2(op, v1, v2) =>
      toExpression(v1).map2(toExpression(v2))(function2(op))
    case f @ OperatorN(op, as) =>
      val args = sequence(as.map(toExpression)).map(_.toArray)
      args.map{ xs: Array[Int] =>
        xs.length match {
          case 1 => function1(op)(xs.head)
          case 2 => function2(op)(xs.head, xs(2))
          case _ => throw new UnsupportedOperationException(f.toString)
        }
      }
  }

  def unit[A, B](b: => B): Expression[A, B] = Expression(_ => b)

  def sequence[A, B](ls: List[Expression[A, B]]): Expression[A, List[B]] =
    ls.foldRight(unit[A, List[B]](List.empty))(
                 (e, acc) => Expression(x => e.eval(x) :: acc.eval(x)))
}

object FunctionRegister {
  val function1: Map[String, Int => Int] = Map()

  val function2: Map[String, (Int, Int) => Int] = Map(
    "+" -> (_ + _),
    "-" -> (_ - _),
    "*" -> (_ * _),
    "/" -> (_ / _),
    "**" -> ((a, b) => (1 /: (for (_ <- 1 to b) yield a))(_ * _))
  )
}
