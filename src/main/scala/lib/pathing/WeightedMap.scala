package lib.pathing

import scala.collection.mutable.PriorityQueue

private final case class TraversalState[Key](cost: Double, steps: List[Key]) extends Ordered[TraversalState[Key]] {
    def compare(that: TraversalState[Key]): Int = that.cost.compare(this.cost)
}

final class WeightedMap[Key](val paths: Map[Key, List[(Double, Key)]] = Map.empty) {

    def addPath(path: WeightedPath[Key]): WeightedMap[Key] = {
        val list  = paths.getOrElse(path.from, List.empty);
        val nList = (path.weight, path.to) :: list ::: Nil
        val nPath = this.paths + (path.from -> nList);
        new WeightedMap(nPath)
    }

    type Steps = List[Key]
    type Path  = (Double, List[Key])

    def pathBetween(start: Key, end: Key): Option[Double] = {
        dijkstra(
          List((0.0, List(start))),
          end,
          Set(start)
        )
    }

    private def dijkstra(remaining: List[Path], dest: Key, visited: Set[Key]): Option[Double] = remaining match {
        case (dist, path) :: remaining_rest =>
            path match {
                case key :: path_rest => {
                    if (key == dest) {
                        Option(dist)
                    }
                    else {
                        val nRem       = paths(key).flatMap { case (d, key) =>
                            if (!visited.contains(key)) { List((dist + d, key :: path)) }
                            else { Nil }
                        }
                        val sortedNRem = (nRem ++ remaining_rest).sortWith { case ((d1, _), (d2, _)) => d1 < d2 }
                        dijkstra(sortedNRem, dest, visited + key)
                    }
                }
            }

        case Nil => Option.empty
    }
}
