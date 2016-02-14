package by.geo.point

/**
  * Узел.
  *
  * @param lat широта в градусах
  * @param lon долгота в градусах
  * @param i   первый индекс
  * @param j   второй индекс
  */
case class Node(lat: Double, lon: Double, i: Int, j: Int) extends Geodetic {
  /**
    * Широта в градусах.
    *
    * @return широта в градусах
    */
  override def latDeg(): Double = lat

  /**
    * Долгота в градусах.
    *
    * @return долгота в градусах
    */
  override def lonDeg(): Double = lon
}
