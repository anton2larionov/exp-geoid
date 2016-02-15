package by.geo.math

import java.util
import java.util.Collections

import by.geo.grav.GravityFieldModel
import by.geo.point.Geodetic
import org.apache.commons.math3.util.FastMath

import scala.collection.concurrent.TrieMap

/**
  * Калькулятор высот геоида.
  */
class GeoidCalculator(private val model: GravityFieldModel) extends GeodeticToDouble {

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
  import scala.collection.convert.decorateAsScala._

  private val polynomsMap = Collections.synchronizedMap(
    new util.WeakHashMap[Double, LegendrePolynoms]()).asScala // иначе OutOfMemoryError

  private val mapSinCos = new TrieMap[Double, (Array[Double], Array[Double])]

  override def apply(pt: Geodetic): Option[Double] = {
    val option = for {
      pt <- Option(pt)
      r <- ellRad(pt)
      phi <- centLat(pt)
      gamma0 <- gamma(pt)
      lon = pt.lonRad()
      scale = model.a / r
    } yield {

      lazy val pairs = {
        val sinLon = Array.ofDim[Double](nMax + 1)
        val cosLon = Array.ofDim[Double](nMax + 1)

        for {m <- 0 to nMax} {
          sinLon(m) = FastMath.sin(lon * m)
          cosLon(m) = FastMath.cos(lon * m)
        }
        (sinLon, cosLon)
      }

      // этап кеширования
      val legendre = polynomsMap.getOrElseUpdate(phi, new LegendrePolynoms(phi, nMax))
      val (sinLon, cosLon) = mapSinCos.getOrElseUpdate(phi, pairs)

      // этап вычислений
      val res = Iterable.range(2, nMax + 1)
        .foldLeft(Option(0.0))((sum, n) => {

          val inner = Iterable.range(0, n + 1)
            .foldLeft(Option(0.0))((sigma, m) => for {
              s <- sigma
              x <- legendre.value(n, m)
            } yield
              s + (model.getC(n, m) * cosLon(m)
                + model.getS(n, m) * sinLon(m)) * x)
          for {
            s <- sum
            i <- inner
          } yield s + (i * FastMath.pow(scale, n))
        }
        )

      for {
        f <- res
      } yield f * (model.GM / (r * gamma0)) + h0
    }
    option flatten
  }
}
