package by.geo.point

import org.scalatest.FunSuite

/**
  * Created on 13.02.2016.
  */
class GridTest extends FunSuite {

  val grid = new Grid(51, 23, 57, 33, 2.5 / 60.0, 2.5 / 60.0)

  test("grid is not eq null") {
    assert(grid != null)
  }

  test("grid is not created") {
    intercept[IllegalArgumentException] {
      new Grid(51, 23, 27, 13, 2.5 / 60.0, 2.5 / 60.0)
    }
  }

  test("grid's columns and rows") {
    assert(grid.colNum == 145)
    assert(grid.rowNum == 241)
  }

  test("grid's values {set; get}") {

    val num = 12.67

    grid.setValue(5, 6, num) match {
      case Some(x) => assert(x == num)
      case None => fail()
    }

    grid.getValue(5, 6) match {
      case Some(x) => assert(x == num)
      case None => fail()
    }

    grid.setValue(-1, -1, num) match {
      case Some(x) => fail()
      case None => assert(true)
    }

    grid.getValue(-1, -1) match {
      case Some(x) => fail()
      case None => assert(true)
    }
  }

  test("grid's extent") {

    val pt1 = new Geodetic {
      override def lonDeg(): Double = 27

      override def latDeg(): Double = 57
    }

    val pt2 = new Geodetic {
      override def lonDeg(): Double = 40

      override def latDeg(): Double = 20
    }

    assert(grid.isValid(pt1))
    assert(!grid.isValid(pt2))
  }
}
