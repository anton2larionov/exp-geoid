package by.geo.math

import org.apache.commons.math3.util.FastMath

/**
  * Полностью нормализованные присоединенные полиномы Лежандра.
  *
  * @param phi  геоцентрическая широта
  * @param nMax максимальная степень
  * @throws IllegalArgumentException если nMax < 1
  */
class LegendrePolynoms(val phi: Double, val nMax: Int) {

  require(nMax >= 1)

  private val t = FastMath.sin(phi)
  private val u = FastMath.cos(phi)

  /**
    * Двухмерный массив значений.
    */
  private[this] val vals: Array[Array[Double]] = Array.ofDim(nMax + 1)

  for {i <- vals.indices} vals(i) = Array.ofDim(i + 1)
  createPolynoms()

  /**
    * Получить значение P[n][m].
    *
    * @param n степень
    * @param m порядок
    * @return значение P[n][m]
    */
  def value(n: Int, m: Int): Option[Double] = {

    if (n < 0 || n >= vals.length) if (m < 0 || m > n) return None

    Some(vals(n)(m))
  }

  private def createPolynoms() {
    // стартовые значения
    vals(0)(0) = 1.0
    vals(1)(0) = init(1, 0)
    vals(1)(1) = u * FastMath.sqrt(3.0)

    for {
      n <- vals.indices
      if n > 1
      m <- vals(n).indices
    } vals(n)(m) = init(n, m)
  }

  // Реализация реккурентного соотношения
  private def init(n: Int, m: Int): Double = {
    // sectoral
    if (n == m) {
      u * FastMath.sqrt((2.0 * m + 1.0) / (2.0 * m)) * vals(n - 1)(m - 1)
    } else {
      // tesseral
      getA(n, m) * getFromArray(n - 1, m) * t - getB(n, m) * getFromArray(n - 2, m)
    }
  }

  private def getFromArray(n: Int, m: Int): Double = {
    if (n < m) {
      0.0
    } else {
      vals(n)(m)
    }
  }

  private def getA(n: Int, m: Int): Double = {
    FastMath.sqrt(((2.0 * n - 1.0) * (2.0 * n + 1.0)) / ((n - m) * (n + m)))
  }

  private def getB(n: Int, m: Int): Double = {
    FastMath.sqrt(((2.0 * n + 1.0) * (n + m - 1.0) * (n - m - 1.0))
      / ((n - m) * (n + m) * (2.0 * n - 3.0)))
  }
}
