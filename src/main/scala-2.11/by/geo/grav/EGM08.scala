package by.geo.grav

import java.io.IOException
import java.nio.file.{Files, Paths}

import by.geo.ref.Ellipsoid

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * EGM08.
  *
  * <p>An Earth Gravitational Model is complete to spherical harmonic
  * degree and order 2159, and contains additional coefficients extending
  * to degree 2190 and order 2159. </p>
  *
  * <table summary="">
  * <tr><td>earth_gravity_constant</td>    <td>0.3986004415E+15</td></tr>
  * <tr><td>radius</td>                    <td>0.63781363E+07</td></tr>
  * <tr><td>max_degree</td>                <td>2190</td></tr>
  * </table>
  *
  */
class EGM08(fileName: String, ell: Ellipsoid)
  extends GravityFieldModel(fileName, ell, maxDegree = 2190) {

  override def GM: Double = 3.986004415E+14

  override def a: Double = 6378136.3

  private def parseDouble(s: String) = try {
    s.toDouble
  } catch {
    case NonFatal(_) => 0.0
  }

  override def readGFC(): Unit = {

    val res = for {
      br <- Try {
        Files.newBufferedReader(Paths.get(fileName))
      }
    } yield Seq(
      Try {
        var (x, y) = (2, 0)

        for (line <-
             Iterator
               .continually(br.readLine())
               .takeWhile(_ != null)
               .filter(_.contains("gfc"))) {

          val raw = line.split("\\s++")

          setC(x, y, parseDouble(raw(3)))
          setS(x, y, parseDouble(raw(4)))

          setErrorC(x, y, parseDouble(raw(5)))
          setErrorS(x, y, parseDouble(raw(6)))

          y = y + 1
          if (y > x) {
            x = x + 1
            y = 0
          }
        }
      },
      Try(br.close())
    )

    // must remake!
    res match {
      case Failure(e) => throw e
      case Success(results) =>
        results.foreach { result =>
          result.recover {
            case e: IOException => throw e
          }
        }
    }
  }
}
