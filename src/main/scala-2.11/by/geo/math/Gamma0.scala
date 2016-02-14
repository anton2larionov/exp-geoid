package by.geo.math

import by.geo.point.Geodetic
import by.geo.ref.Ellipsoid
import org.apache.commons.math3.util.FastMath

/**
  * Функция вычисления нормальной силы тяжести на поверхности эллипсоида.
  */
class Gamma0(ell: Ellipsoid) extends GeodeticToDouble {

  /**
    * Вычисление нормальной силы тяжести на поверхности эллипсоида
    * в точке pt.
    *
    * @return нормальная сила тяжести
    */
  override def apply(pt: Geodetic): Option[Double] = {
    for {
      pt <- Option(pt)
      ell <- Option(ell)
    } yield {
      val lat = pt.latRad()

      ell.gammaE * (1 + ell.k * FastMath.pow(FastMath.sin(lat), 2)) /
        FastMath.sqrt(1 - FastMath.pow(ell.e1 * FastMath.sin(lat), 2))
    }
  }
}
