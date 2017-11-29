package io.github.foodzee.math.galois

import io.github.foodzee.math.Field

/**
  * @author foodzee.
  */
class GaloisField(p: Int, n: Int, poly: Int) extends Field[Int] {
  require(p == 2 && n == 4, "Only GF(2^4) is supported for now.")

  /** Multiplication table. */
  private val m =
    for (a <- 0 to 0xF; b <- 0 to 0xF) yield {
      var c = 0

      // Bitwise ("peasant") multiplication, using galois addition
      for (bit <- 3 to (0, -1)) {
        if ((b & (1<<bit)) != 0) {
          c = add(c, a << bit)
        }
      }

      // Reduce back into galois field (can also be folded into above loop)
      val high = Integer.highestOneBit(poly)
      for (bit <- 3 to (0, -1)) {
        if ((c & (high << bit)) != 0) {
          c = sub(c, poly << bit)
        }
      }

      c
    }
  assert(m.size == 16*16)

  def asInt(a: Int) = a

  def add(a: Int, b: Int): Int = a ^ b
  def neg(a: Int): Int = a

  def mul(a: Int, b: Int): Int = {
    require(0 <= a && a <= 0xF)
    require(0 <= b && b <= 0xF)
    m(a + b*16)
  }

  def inv(a: Int): Int = {
    if (a == 0) throw new ArithmeticException("0 has no inverse")
    val i = 1 to 0xF find (b => mul(a, b) == 1)
    assert(i.isDefined, a)
    i.get
  }

  def printMultiplicationTable(): Unit = {
    for (a <- -1 to 0xF) {
      for (b <- -1 to 0xF) {
        if (a == -1) {
          if (b == -1) {
            printf(" * | ")
          } else {
            printf("%x ", b)
          }
        } else if (b == -1) {
          printf(" %x | ", a)
        } else {
          printf("%x ", mul(a, b))
        }
      }
      println()
    }
  }
}
