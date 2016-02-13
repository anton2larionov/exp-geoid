package by.geo.math

import by.geo.point.{Geodetic, Grid}
import org.scalatest.FunSuite

/**
  * Created on 13.02.2016.
  */
class BilinearInterpolatorTest extends FunSuite {

  test("testApply") {
    val grid = new Grid(0, 0, 2, 2, 1, 1)
    val inter = new BilinearInterpolator(grid)

    val pt = new Geodetic {
      override def latDeg(): Double = 0.5

      override def lonDeg(): Double = 0.5
    }

    inter(pt) match {
      case Some(x) => assert(x == 0)
      case None => fail()
    }

    grid.setValue(0, 0, 0.0)
    grid.setValue(0, 1, 2.0)
    grid.setValue(1, 0, 2.0)
    grid.setValue(1, 1, 4.0)

    inter(pt) match {
      case Some(x) => assert(x == 2)
      case None => fail()
    }

  }

}
