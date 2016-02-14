package by.geo.point

/**
  * Грид (регулярная сетка двухмерных точек)
  * на основе двухмерного массива значений.
  *
  * @param lat0 минимальная широта
  * @param lon0 минимальная долгота
  * @param lat  максимальная широта
  * @param lon  максимальная долгота
  * @param dlat шаг по широте
  * @param dlon шаг по долготе
  * @throws IllegalArgumentException если (lat < lat0 || lon < lon0)
  */
case class Grid(lat0: Double, lon0: Double, lat: Double, lon: Double, dlat: Double, dlon: Double) {

  require(lat >= lat0)
  require(lon >= lon0)
  require(dlat > 0)
  require(dlon > 0)

  /**
    * Число столбцов.
    */
  val colNum: Int = ((lat - lat0 + dlat / 2) / dlat + 1).toInt

  /**
    * Число строк.
    */
  val rowNum: Int = ((lon - lon0 + dlat / 2) / dlon + 1).toInt

  /**
    * Двухмерный массив значений.
    */
  private[this] val vals: Array[Array[Double]] = Array.ofDim[Double](rowNum, colNum)

  /**
    * @param pt геодезические координаты
    * @return true если координаты pt в территориальных рамках грида,
    *         иначе false
    */
  def isValid(pt: Geodetic): Boolean = {
    val B = pt.latDeg()
    val L = pt.lonDeg()
    !(B < lat0 || B > lat || L < lon0 || L > lon)
  }

  private def testIJ(i: Int, j: Int): Boolean = i >= 0 && i < rowNum && j >= 0 && j < colNum

  /**
    * Задать значение для узла грида.
    *
    * @param i     номер ряда
    * @param j     номер столбца
    * @param value значение
    * @return значение из узла грида
    */
  def setValue(i: Int, j: Int, value: Double): Option[Double] = {
    if (testIJ(i, j)) {
      vals(i)(j) = value
      Some(value)
    } else {
      None
    }
  }

  /**
    * Получить значение из узла грида.
    *
    * @param i номер строки
    * @param j номер столбца
    * @return значение из узла грида
    */
  def getValue(i: Int, j: Int): Option[Double] = {
    if (testIJ(i, j)) {
      Some(vals(i)(j))
    } else {
      None
    }
  }

}
