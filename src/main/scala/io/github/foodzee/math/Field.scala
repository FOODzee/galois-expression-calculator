package io.github.foodzee.math

/**
  * Definition of field operations.
  *
  * @author foodzee.
  */
trait Field[E] {
  def U: E

  def neg(a: E): E
  def inv(a: E): E

  def add(a: E, b: E): E
  def sub(a: E, b: E): E = add(a, neg(b))
  def mul(a: E, b: E): E
  def div(a: E, b: E): E = mul(a, inv(b))

  def pow(a: E, n: Int): E = (U /: (for (_ <- 1 to n) yield a))(mul)
  def asInt(a: E): Int
}

object RealField extends Field[Double] {
  def U = 1.0
  def asInt(a: Double) = a.toInt

  override def neg(a: Double) = -a
  override def inv(a: Double) = 1/a

  override def add(a: Double, b: Double) = a + b
  override def sub(a: Double, b: Double) = a - b
  override def mul(a: Double, b: Double) = a * b
  override def div(a: Double, b: Double) = a / b
}