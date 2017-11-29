package io.github.foodzee.math

import io.github.facaiy.math.expression.MathExp.parse
import io.github.foodzee.math.galois.GaloisField

import scala.collection.mutable

/**
  * @author ijorch
  */
object EllipticCurve extends App {

  implicit val GF = new GaloisField(2, 4, 0x13) // 0x13 == 0b10011 == x^4 + x + 1

  // elliptic curve equation: left- and right-hand sides
  val left = parse("$y ** 2 + $x * $y")
  val right = parse("$x ** 3 + $b * $x ** 2 + 1")

  val g = 2 // primitive element of GF
  val b = GF.pow(g, 4) // parameter of curve

  /** point in projective plane */
  case class Point(x: Int, y: Int, z: Int = 1) {
    override def toString = f"($x%x, $y%x)"
  }
  val ZERO = Point(0, 0)

  /** set of points of elliptic curve */
  val points: Seq[Point] = ZERO +: (
    for {
      x <- 0 to 0xF
      y <- 0 to 0xF
      vars = Map("b" -> b, "x" -> x, "y" -> y)
      if left.eval(vars) == right.eval(vars)
    } yield Point(x, y)
    )

  println(points map (_.toString) mkString "; ")

  // following methods are definitions of addition in the group of defined elliptic curve
  // they came from paper https://link.springer.com/content/pdf/10.1007/3-540-46416-6_27.pdf
  // TODO: extract to special class (together with points: Seq)

  /** @return P + P */
  def projectiveX2(P: Point): Point = {
    val vars = mutable.Map("b" -> b,
      "x" -> P.x, "y" -> P.y, "z" -> P.z
    )

    vars += ("A" -> parse("$y * $z + $x**2").eval(vars))

    val x2z2x = parse("$A**2 + $x * $z * $A + $b * $x**2 * $z**2")
    val x3z3y = parse("$x**5 * $z + ($A + $x * $z) * $x2z2x")
    val x3z3 = parse("$x**3 * $z**3")

    val xp = x2z2x.eval(vars)
    vars ++= Map(
      "xp" -> xp,
      "yp" -> x3z3y.eval(vars + ("x2z2x" -> xp)),
      "zp" -> x3z3.eval(vars)
    )

    if (vars("zp") == 0) ZERO else {
      Point(
        x = parse("$xp * $x * $z / $zp").eval(vars),
        y = parse("$yp / $zp").eval(vars)
      )
    }
  }

  /** @return P1 + P2 */
  def projectiveSum(P1: Point, P2: Point): Point = {
    if (P1 == ZERO) return P2
    if (P2 == ZERO) return P1
    if (P1 == P2) return projectiveX2(P1)

    val vars = mutable.Map("b" -> b,
      "x1" -> P1.x, "y1" -> P1.y, "z1" -> P1.z,
      "x2" -> P2.x, "y2" -> P2.y, "z2" -> P2.z
    )

    val A = parse("$z2 * $x1 + $x2")
    val B = parse("$z2 * $y1 + $y2")

    vars ++= Map(
      "A" -> A.eval(vars),
      "B" -> B.eval(vars)
    )

    val zA2x = parse("$z2 * $B**2 + $z2 * $A * $B + $A**2 * ($x1 * $z2 + $x2 + $b * $z2)")
    val zA3y = parse("$z2 * $A**2 * ($y1 * $x2 + $x1 * $y2) + ($A + $B) * $zA2x")
    val zA3 = parse("$z2 * $A**3")

    val xp = zA2x.eval(vars)
    val yp = zA3y.eval(vars + ("zA2x" -> xp))
    val zp = zA3.eval(vars)

    if (zp == 0) ZERO else {
      Point(
        x = GF.div(GF.mul(xp, vars("A")), zp),
        y = GF.div(yp, zp)
      )
    }
  }

  def sum = projectiveSum _ // alias-shorthand

  /** @return [m]*P */
  def mul(m: Int, P: Point): Point = Seq.fill(m)(P) reduce sum  // can be more efficient if `projectiveX2` would be used too

  // check that elliptic curve points set with defined addition is closed and hence is a group
  assert((for (p <- points; q <- points) yield sum(p, q)) forall points.contains,
    "sum is broken. Counterexample is " + {
      val Some((p, q, s)) = (for (p <- points; q <- points) yield (p, q, sum(p, q))) find {case (_,_,s) => !points.contains(s) }
      s"sum($p, $q) = $s"
    }
  )
  assert((for (p <- points; m <- 1 to 16) yield mul(m, p)) forall points.contains, "mul is broken")

  // following are calculations for my university assignment

  /** generator of the curve points set */
  val G = (points find { G =>
    points forall { p =>
      (1 to 16) exists (m => mul(m, G) == p)
    }
  }).get

  println("G = " + G)
  val P = mul(3, G)
  println("P = " + P)
  val R = mul(7, G)
  println("R = " + R)

  println("Q = " + mul(7, P))
  println("Q' = " + mul(3, R))
}
