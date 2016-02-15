package by.geo.math

import by.geo.point.Geodetic
import by.geo.ref.Ellipsoid
import org.apache.commons.math3.util.FastMath

/**
  * Функция вычисления локального эллипсоидального радиуса.
  */
class EllipsoidalRadius(private val ell: Ellipsoid) extends GeodeticToDouble {

  /**
    * Вычисление локального эллипсоидального радиуса в точке pt.
    *
    * @return локальный эллипсоидальной радиус
    */
  override def apply(pt: Geodetic): Option[Double] = {
    for {
      pt <- Option(pt)
      ell <- Option(ell)
    } yield {
      val e2 = FastMath.pow(ell.e1, 2)
      val sinPhi2 = FastMath.pow(FastMath.sin(pt.latRad()), 2)

      ell.a * FastMath.sqrt(1 - (e2 * (1 - e2) * sinPhi2 / (1 - e2 * sinPhi2)))
    }
  }
}
