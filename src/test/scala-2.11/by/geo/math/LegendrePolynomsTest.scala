package by.geo.math

import org.scalatest.FunSuite

/**
  * Created by Ennui on 13.02.2016.
  */
class LegendrePolynomsTest extends FunSuite {

  test("init") {
    assert(new LegendrePolynoms(0.5, 2100) != null)
  }

}
