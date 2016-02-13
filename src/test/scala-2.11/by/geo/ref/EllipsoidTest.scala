package by.geo.ref

import org.scalatest.FunSuite

/**
  * Created on 13.02.2016.
  */
class EllipsoidTest extends FunSuite {
  test("init") {
    assert(GRS80 == new Ellipsoid(6378137, 1.0 / 298.257222101, 3.986005000E+14))
    assert(WGS84 == new Ellipsoid(6378137, 1.0 / 298.257223563, 3.986004418E+14))
  }
}
