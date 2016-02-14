package by.geo.grav

import by.geo.ref.Ellipsoid
import org.apache.commons.math3.util.FastMath

/**
  * Глобальная модель гравитационного поля Земли.
  *
  * @param fileName имя файла с коэффициентами модели
  * @param ell      эллипсоид
  * @param nMax     максимальная степень
  */
abstract class GravityFieldModel(val fileName: String,
                                 val ell: Ellipsoid,
                                 val nMax: Int) {
  require(nMax >= 1)

  private val zonDeg = 5
  val W = 62636856.0

  /**
    * Геоцентрическая гравитационная постоянная.
    */
  def GM: Double

  /**
    * Большая полуось.
    */
  def a: Double

  /**
    * Максимальная степень.
    */
  val maxDegree: Int = nMax

  private val C: Array[Array[Double]] = Array.ofDim(nMax + 1)
  private val S: Array[Array[Double]] = Array.ofDim(nMax + 1)
  private val dC: Array[Array[Double]] = Array.ofDim(nMax + 1)
  private val dS: Array[Array[Double]] = Array.ofDim(nMax + 1)
  fillArray(C)
  fillArray(S)
  fillArray(dC)
  fillArray(dS)
  readGFC()
  zonalCorrect()

  protected def readGFC(): Unit

  /**
    * Формирование массивов.
    */
  private def fillArray(arr: Array[Array[Double]]): Unit = {
    for {i <- arr.indices} arr(i) = Array.ofDim(i + 1)
  }

  /**
    * Коррекция зональных коэффициентов.
    */
  private def zonalCorrect() {
    for {i <- 1 to zonDeg} C(2 * i)(0) += getZonalC(i)
  }

  /**
    * Нормированный зональный коэффициент.
    */
  private def getZonalC(n: Int): Double = {
    (ell.GM / GM) * FastMath.pow(ell.a / a, n) * ell.j2n(n) / FastMath.sqrt(4 * n + 1)
  }

  /**
    * Коэффициент C(n)(m).
    */
  def getC(n: Int, m: Int): Double = C(n)(m)

  /**
    * Коэффициент S(n)(m).
    */
  def getS(n: Int, m: Int): Double = S(n)(m)

  /**
    * Ошибка коэффициента C(n)(m).
    */
  def getErrorC(n: Int, m: Int): Double = dC(n)(m)

  /**
    * Ошибка коэффициента S(n)(m).
    */
  def getErrorS(n: Int, m: Int): Double = dS(n)(m)

  /**
    * Задать коэффициент C(n)(m).
    */
  protected def setC(n: Int, m: Int, value: Double) {
    C(n)(m) = value
  }

  /**
    * Задать коэффициент S(n)(m).
    */
  protected def setS(n: Int, m: Int, value: Double) {
    S(n)(m) = value
  }

  /**
    * Задать oшибкy коэффициента C(n)(m).
    */
  protected def setErrorC(n: Int, m: Int, value: Double) {
    dC(n)(m) = value
  }

  /**
    * Задать oшибкy коэффициента S(n)(m).
    */
  protected def setErrorS(n: Int, m: Int, value: Double) {
    dS(n)(m) = value
  }

}
