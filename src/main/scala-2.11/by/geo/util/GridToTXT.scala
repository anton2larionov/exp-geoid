package by.geo.util

import java.nio.file.{Files, Paths}

import by.geo.point.Grid

/**
  * Преобразователь грида в текстовый формат.
  */
class GridToTXT(grid: Grid) {

  def write(fileName: String): Unit = {

    val lon0 = grid.lon0
    val lat0 = grid.lat0

    val dlon = grid.dlon
    val dlat = grid.dlat

    val w = Files.newBufferedWriter(Paths.get(fileName))

    try {
      for {
        i <- (0 until grid.rowNum).reverse
        j <- 0 until grid.colNum
        h <- grid.getValue(i, j)
      } {
        val b = new StringBuilder()
        b.append(f"${lon0 + dlon * j}%.12f\t")
          .append(f"${lat0 + dlat * i}%.12f\t")
          .append(f"$h%.5f\n")
        w.write(b.toString().replace(",", "."))
      }
    } finally {
      w.close()
    }
  }
}
