package by.geo.math

import by.geo.point.{Geodetic, Grid}

/**
  * Функция, осуществляющая билинейную интерполяцию по сетке значений.
  */
class BilinearInterpolator(grid: Grid) extends GeodeticToDouble {

  override def apply(pt: Geodetic): Option[Double] = {

    val res = for {
      grid <- Option(grid)
      pt <- Option(pt) if grid.isValid(pt)
    } yield {
      val B = pt.latDeg()
      val L = pt.lonDeg()
      val i = ((B - grid.lat0) / grid.dlat).toInt
      val j = ((L - grid.lon0) / grid.dlon).toInt

      val fx = (L - grid.lon0) / grid.dlon - j
      val fy = (B - grid.lat0) / grid.dlat - i

      val f = if (i < (grid.rowNum - 1)) i + 1 else i
      val g = if (j < (grid.colNum - 1)) j + 1 else j

      for {
        d1 <- grid.getValue(i, j)
        d2 <- grid.getValue(f, j)
        d3 <- grid.getValue(i, g)
        d4 <- grid.getValue(f, g)
      } yield (1 - fx) * (1 - fy) * d1 + (1 - fx) * fy * d2 + fx * (1 - fy) * d3 + fx * fy * d4
    }

    res getOrElse None
  }
}
