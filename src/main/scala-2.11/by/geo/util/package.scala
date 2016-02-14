package by.geo

import by.geo.point.{Grid, Node}

/**
  * Created by Ennui on 14.02.2016.
  */
package object util {
  type GridToNodesFun = Grid => List[Node]
}
