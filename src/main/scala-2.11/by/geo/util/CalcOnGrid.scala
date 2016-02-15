package by.geo.util

import by.geo.math.GeoidCalculator
import by.geo.point.Grid

/**
  * Вычисление высот геоида для узлов регулярной сетки.
  *
  * @param grid регулярная сетка
  * @param calc калькулятор высот геоида
  */
class CalcOnGrid(private val grid: Grid, private val calc: GeoidCalculator) {

  def perform(): Unit = {
    val gridToNodes = new GridToNodes
    val parNodes = gridToNodes(grid).par

    lazy val heights = parNodes.map(calc(_))

    parNodes
      .zip(heights)
      .foreach(pair => {
        val (node, h) = pair
        grid.setValue(node.i, node.j, h getOrElse 0.0)
      })
  }
}
