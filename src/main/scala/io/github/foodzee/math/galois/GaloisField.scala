package io.github.foodzee.math.galois

import io.github.foodzee.math.Field

/**
  * @author foodzee.
  */
class GaloisField(p: Int, n: Int, poly: Int) extends Field {
  assert(p == 2 && n == 4, "Only GF(2^4) is supported for now.")

  /** Multiplication table. */
  val m =
    for (a <- 0 until 0xF;
         b <- 0 until 0xF)
      yield {
        var c = 0

        // Bitwise ("peasant") multiplication, using galois addition
        for (bit <- 3 to 0) {
          if ((b & (1<<bit)) != 0) {
            c = add(c, a << bit)
          }
        }

        // Reduce back into galois field (can also be folded into above loop)
        val high = Integer.highestOneBit(poly)
        for (bit <- 3 to 0) {
          if ((c & (high << bit)) != 0) {
            c = sub(c, poly << bit)
          }
        }

        c
      }

  type E = Int

  def add(a: E, b: E): E = a ^ b
  def neg(a: E): E = a

  def mul(a: E, b: E): E = ???
  def inv(a: E): E = ???
}
