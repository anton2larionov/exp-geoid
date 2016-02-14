package by.geo.util

import by.geo.math.GeoidCalculator
import by.geo.point.Grid

/**
  * Вычисление высот геоида для узлов регулярной сетки.
  *
  * @param grid регулярная сетка
  * @param calc калькулятор высот геоида
  */
class CalcOnGrid(grid: Grid, calc: GeoidCalculator) {

  def perform(): Unit = {
    val gridToNodes = new GridToNodes

    gridToNodes(grid)
      .par
      .map(node => (node, calc(node)))
      .foreach(pair => {
        val node = pair._1
        val h = pair._2 getOrElse 0.0

        grid.setValue(node.i, node.j, h)
      })
  }
}
