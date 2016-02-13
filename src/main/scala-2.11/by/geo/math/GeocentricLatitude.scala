package by.geo.math

import by.geo.point.Geodetic
import by.geo.ref.Ellipsoid
import org.apache.commons.math3.util.FastMath

/**
  * Функция вычисления сферической широты.
  */
class GeocentricLatitude(ell: Ellipsoid) extends GeodeticToDouble {

  /**
    * Вычисление сферической широты в точке pt.
    *
    * @return сферическая широта
    */
  override def apply(pt: Geodetic): Option[Double] = {
    for {
      pt <- Option(pt)
      ell <- Option(ell)
    } yield {
      FastMath.atan(FastMath.pow(ell.b / ell.a, 2) * FastMath.tan(pt.latRad()))
    }
  }
}
