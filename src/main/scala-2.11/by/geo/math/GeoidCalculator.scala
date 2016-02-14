package by.geo.math

import java.util
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import by.geo.grav.GravityFieldModel
import by.geo.point.Geodetic
import org.apache.commons.math3.util.FastMath

/**
  * Калькулятор высот геоида.
  */
class GeoidCalculator(model: GravityFieldModel) extends GeodeticToDouble {

  private val ell = model.ell
  private val nMax = model.maxDegree
  private val ellRad = new EllipsoidalRadius(ell)
  private val centLat = new GeocentricLatitude(ell)
  private val gamma = new Gamma0(ell)

  /**
    * Андуляция геоида нулевого порядка
    */
  val h0 = (model.GM - ell.GM) / (ell.rMean * ell.gammaMean) - (model.W - ell.U) / ell.gammaMean

  // Cache
  private val polynomsMap = Collections.synchronizedMap(
    new util.WeakHashMap[Double, LegendrePolynoms]())
  // иначе OutOfMemoryError
  private val mapSinCos = new ConcurrentHashMap[Double, (Array[Double], Array[Double])]()

  override def apply(pt: Geodetic): Option[Double] = {
    val res = for {
      pt <- Option(pt)
      r <- ellRad(pt)
      phi <- centLat(pt)
      gamma0 <- gamma(pt)
    } yield {
      val lon = pt.lonRad()
      val scale = model.a / r

      lazy val f1: util.function.Function[Double, LegendrePolynoms] =
        new util.function.Function[Double, LegendrePolynoms] {
          override def apply(key: Double): LegendrePolynoms = new LegendrePolynoms(phi, nMax)
        }

      lazy val f2: util.function.Function[Double, (Array[Double], Array[Double])] =
        new util.function.Function[Double, (Array[Double], Array[Double])] {
          override def apply(key: Double): (Array[Double], Array[Double]) = {
            val sinLon = Array.ofDim[Double](nMax + 1)
            val cosLon = Array.ofDim[Double](nMax + 1)

            for {
              m <- sinLon.indices
            } {
              sinLon(m) = FastMath.sin(lon * m)
              cosLon(m) = FastMath.cos(lon * m)
            }
            (sinLon, cosLon)
          }
        }

      // этап кеширования
      val legendre = polynomsMap.computeIfAbsent(phi, f1)
      val sincos = mapSinCos.computeIfAbsent(phi, f2)

      val sinLon = sincos._1
      val cosLon = sincos._2

      // этап вычислений
      var N = 0.0

      for {n <- 2 to nMax} {
        var sigma = 0.0
        for {
          m <- 0 to n
          x <- legendre.value(n, m)
        } {
          sigma += (model.getC(n, m) * cosLon(m)
            + model.getS(n, m) * sinLon(m)) * x

          //  println((model.getC(n, m), model.getS(n, m)))
        }
        N += (sigma * FastMath.pow(scale, n))
      }
      N *= (model.GM / (r * gamma0))
      N += h0
      return Option(N)
    }
    res
  }
}
