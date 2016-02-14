package by.geo.util

import by.geo.point.{Grid, Node}

/**
  * Преобразователь грида в коллекцию узлов.
  */
class GridToNodes extends GridToNodesFun {
  override def apply(grid: Grid): List[Node] = {

    val res = for {
      grid <- Option(grid)
    } yield {
      val nodes = for {
        i <- 0 until grid.rowNum
        j <- 0 until grid.colNum
      } yield {
        new Node(grid.lat0 + grid.dlat * i, grid.lon0 + grid.dlon * j, i, j)
      }
      nodes.toList
    }
    res getOrElse Nil
  }
}
