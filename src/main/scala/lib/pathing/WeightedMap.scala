package lib.pathing

import scala.collection.mutable.PriorityQueue

private final case class TraversalState[Key](cost: Double, steps: List[Key]) extends Ordered[TraversalState[Key]] {
    def compare(that: TraversalState[Key]): Int = that.cost.compare(this.cost)
}

final class WeightedMap[Key](val paths: Map[Key, List[WeightedPath[Key]]] = Map.empty) {

    def addPath(path: WeightedPath[Key]): WeightedMap[Key] = {
        val list  = paths.getOrElse(path.from, List.empty[WeightedPath[Key]]);

        val nPath = this.paths + (path.from -> (list ::: path :: Nil));

        new WeightedMap(nPath)
    }

    type Steps = List[Key]

    def pathBetween(start: Key, end: Key): Option[(Double, List[Key])] = {
        dijkstra(
          PriorityQueue(new TraversalState(0.0, List(start))),
          end,
          Set(start)
        )
    }

    private def updateQueue(
        queue: PriorityQueue[TraversalState[Key]],
        currCost: Double,
        currSteps: Steps,
        visited: Set[Key]
    ): PriorityQueue[TraversalState[Key]] = {
        (currSteps: @unchecked) match {
            case keys @ key :: _ => {
                paths(key)
                    .filter(k => !visited.contains(k.to))
                    .map(k => TraversalState(k.weight + currCost, k.to :: keys))
                    .foreach(p => queue.enqueue(p))
                queue
            }
        }
    }

    private def dijkstra(
        queue: PriorityQueue[TraversalState[Key]],
        end: Key,
        visited: Set[Key] = Set.empty
    ): Option[(Double, List[Key])] = {
        if (queue.isEmpty) {
            Option.empty
        }
        else {
            queue.dequeue() match {
                case m =>
                    (m.cost, m.steps) match {
                        case (dist, steps @ `end` :: _)    => {
                            Option(dist, steps.reverse)
                        }
                        case (dist, steps @ currStep :: _) => {
                            val nQueue = updateQueue(queue, dist, steps, visited)
                            dijkstra(
                              nQueue,
                              end,
                              visited + currStep
                            )
                        }
                        case _ => Option.empty
                    }
            }
        }
    }
}
