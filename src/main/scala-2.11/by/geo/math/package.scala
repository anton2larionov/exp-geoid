package by.geo

/**
  * Created on 13.02.2016.
  */
package object math {

  import by.geo.point.Geodetic

  type GeodeticToDouble = Geodetic => Option[Double]
}
