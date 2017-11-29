package io.github.foodzee.math

/**
  * @author ijorch
  */
object EllipticCurve extends App {
  import io.github.facaiy.math.expression.MathExp.parse
  import io.github.foodzee.math.galois.GaloisField

  import scala.collection.mutable

  implicit val GF = new GaloisField(2, 4, 0x13) // 0x13 == 0b10011 == x^4 + x + 1

  val left = parse("$y ** 2 + $x * $y")
  val right = parse("$x ** 3 + $b * $x ** 2 + 1")

  val g = 2
  val b = GF.pow(g, 4)

  case class Point(x: Int, y: Int, z: Int = 1) {
    override def toString = f"($x%x, $y%x)"
  }
  val ZERO = Point(0, 0)

  // points in projective plane
  val points: Seq[Point] = ZERO +: (
    for {
      x <- 0 to 0xF
      y <- 0 to 0xF
      vars = Map("b" -> b, "x" -> x, "y" -> y)
      if left.eval(vars) == right.eval(vars)
    } yield Point(x, y)
    )

  println(points map (_.toString) mkString "; ")

  def fairSum(P: Point, Q: Point): Point = { // incorrect
    P match { case Point(0, 0, _) => return Q; case _ => }
    Q match { case Point(0, 0, _) => return P; case _ => }

    val vars = mutable.Map("b" -> b,
      "x1" -> P.x, "y1" -> P.y,
      "x2" -> Q.x, "y2" -> Q.y
    )

    val (l, nu) = if (P.x != Q.x) {
      ( parse("($y2 - $y1) / ($x2 - $x1)"),
        parse("($y1 * $x2 - $y2 * $x1) / ($x2 - $x1)")
      )
    } else if (parse("$y1 + $y2 + $x1").eval(vars) != 0) {
      ( parse("(3 * $x1**2 + 2 * $b * $x1 - $y1) / (2*$y1 + $x1)"),
        parse("($x1 ** 3 + 2) / (2*$y1 + $x1)")
      )
    } else {
      return ZERO
    }

    vars ++= Map(
      "l" -> l.eval(vars),
      "nu" -> nu.eval(vars)
    )

    val x3 = parse("$l ** 2 + $l - $b - $x1 - $x2")
      .eval(vars)
    val y3 = parse("($l + 1)*$x3 - $nu")
      .eval(vars + ("x3" -> x3))

    Point(x3, y3)
  }
  def fairMul(m: Int, P: Point): Point = Seq.fill(m)(P) reduce sum

  def projectiveDouble(P: Point): Point = {
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
  def projectiveSum(P1: Point, P2: Point): Point = {
    if (P1 == ZERO) return P2
    if (P2 == ZERO) return P1
    if (P1 == P2) return projectiveDouble(P1)

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

  def sum = projectiveSum _
  def mul = fairMul _

  assert((for (p <- points; q <- points) yield sum(p, q)) forall points.contains,
    "sum is broken. Counterexample is " + {
      val Some((p, q, s)) = (for (p <- points; q <- points) yield (p, q, sum(p, q))) find {case (_,_,s) => !points.contains(s) }
      s"sum($p, $q) = $s"
    }
  )
  assert((for (p <- points; m <- 1 to 16) yield mul(m, p)) forall points.contains, "mul is broken")

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
