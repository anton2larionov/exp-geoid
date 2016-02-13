package by.geo.point

/**
  * Объект, имеющий Геодезические координаты.
  */
trait Geodetic {

  /**
    * Широта в градусах.
    *
    * @return широта в градусах
    */
  def latDeg(): Double

  /**
    * Долгота в градусах.
    *
    * @return долгота в градусах
    */
  def lonDeg(): Double

  import org.apache.commons.math3.util.FastMath

  /**
    * Широта в радианах.
    *
    * @return широта в радианах
    */
  def latRad(): Double = FastMath toRadians latDeg


  /**
    * Долгота в радианах.
    *
    * @return долгота в радианах
    */
  def lonRad(): Double = FastMath toRadians lonDeg

}
