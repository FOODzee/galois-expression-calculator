package io.github.foodzee.math

/**
  * Definition of field operations.
  *
  * @author foodzee.
  */
trait Field {
  type E

  def neg(a: E): E
  def inv(a: E): E

  def add(a: E, b: E): E
  def sub(a: E, b: E): E = add(a, neg(b))
  def mul(a: E, b: E): E
  def div(a: E, b: E): E = mul(a, inv(b))
}

object RealField extends Field {
  type E = Double

  override def neg(a: E): E = -a
  override def inv(a: E): E = 1/a

  override def add(a: E, b: E): E = a + b
  override def sub(a: E, b: E): E = a - b
  override def mul(a: E, b: E): E = a * b
  override def div(a: E, b: E): E = a / b
}