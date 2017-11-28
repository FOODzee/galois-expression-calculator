package io.github.foodzee.math

import io.github.facaiy.math.expression.MathExp
import io.github.foodzee.math.galois.GaloisField

/**
  * @author ijorch
  */
object Main extends App {
  val gf = new GaloisField(2, 4, 0x13 /*0b10011*/)
  gf.printMultiplicationTable()

  for (n <- 1 to 15) {
    val op = f"$$g ** $n%x"
    val ex = MathExp.parse(op)
    val res = ex.eval(Map("g" -> 2))
    println(f"$op = $res%x")
  }
}
