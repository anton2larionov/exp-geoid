package by.geo

import by.geo.grav.EGM08
import by.geo.math.{BilinearInterpolator, GeoidCalculator}
import by.geo.point.{Geodetic, Grid}
import by.geo.ref.GRS80
import by.geo.util.{CalcOnGrid, GridToTXT}

/**
  * Created on 14.02.2016.
  */
object Main {

  def main(args: Array[String]) {
    val grid = new Grid(57, 23, 57, 23, 2.5 / 60.0, 2.5 / 60.0)

    for {
      model <- Option(new EGM08("e:/downloads/egm2008.gfc", GRS80))
    } {
      val calc = new GeoidCalculator(model)

      new CalcOnGrid(grid, calc).perform()
      new GridToTXT(grid).write("e:/downloads/out.txt")
    }

    println(
      new BilinearInterpolator(grid)(new Geodetic {
        override def lonDeg(): Double = 23

        override def latDeg(): Double = 57
      })
    )
  }
}
