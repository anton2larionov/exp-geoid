package by.geo.ref

import org.apache.commons.math3.util.FastMath

/**
  * Референц эллипсоид.
  *
  * @param a  большая полуось
  * @param f  сжатие
  * @param GM геоцентрическая гравитационная постоянная
  */
case class Ellipsoid(a: Double, f: Double, GM: Double) {

  val w = 7.292115E-5 // стандартное значение

  /* Производные параметры. */
  private val a2 = FastMath.pow(a, 2)
  private val w2 = FastMath.pow(w, 2)

  val b = a * (1 - f)
  val e = FastMath.sqrt(a2 - FastMath.pow(b, 2))
  val e1 = e / a
  val e2 = e / b

  val U = GM * FastMath.atan(e2) / e + w2 * a2 / 3.0
  val m = w2 * a2 * b / GM

  lazy val q = FastMath.pow(a, 3) * w2 / GM
  private lazy val ee = FastMath.pow(e2, 2)

  lazy val gammaE = GM * (1.0 - 3 * q / 2 + f + FastMath.pow(f, 2) - 3.0 * f * q / 7.0
    + FastMath.pow(f, 3) - 125.0 * q * FastMath.pow(f, 2)
    / 294.0) / a2

  lazy val gammaP = GM / a2 - 2.0 * w2 * b * (1.0 - 2.0 * ee * (e2 - FastMath.atan(e2))
    / ((3.0 + ee) * FastMath.atan(e2) - 3.0 * e2)) / 3.0

  lazy private val q0 = 0.5 * ((1 + 3 / FastMath.pow(e2, 2)) * FastMath.atan(e2) - 3.0 / e2)

  lazy val k = (b * gammaP - a * gammaE) / (a * gammaE)

  private lazy val C = (1.0 / 3.0) * (1.0 - (2.0 / 15.0) * (m * e2 / q0))

  lazy val rMean = (2.0 * a + b) / 3.0
  lazy val gammaMean = (2.0 * gammaE + gammaP) / 3.0

  /**
    * Коэффициент динамического сжатия.
    *
    * @param n степень
    * @return коэффициент динамического сжатия
    */
  def j2n(n: Int): Double = {
    if (n < 0) {
      0.0
    } else {
      FastMath.pow(-1, n + 1) * 3 * FastMath.pow(e1, 2 * n) * (1 - n + 5 * n * C) / ((2 * n + 1) * (2 * n + 3))
    }
  }
}

object GRS80
  extends Ellipsoid(
    a = 6378137,
    f = 1.0 / 298.257222101,
    GM = 3.986005000E+14)

object WGS84
  extends Ellipsoid(
    a = 6378137,
    f = 1.0 / 298.257223563,
    GM = 3.986004418E+14)