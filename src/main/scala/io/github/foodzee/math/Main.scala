package io.github.foodzee.math

import io.github.foodzee.math.galois.GaloisField

/**
  * @author ijorch
  */
object Main extends App {
  val gf = new GaloisField(2, 4, 0x13 /*0b10011*/)
  gf.printMultiplicationTable()

  println(gf.inv(0xD))
}
