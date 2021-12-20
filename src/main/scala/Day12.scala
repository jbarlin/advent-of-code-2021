import lib.DayTemplate
import scala.io.Source
import lib.RecursiveUtils

type Day12Type = Map[String, List[String]];

object Day12 extends DayTemplate[Day12Type] {
    def parseInput(): Day12Type = {
        Source
            .fromResource("day12.txt")
            .getLines
            .filter(!_.isBlank)
            .map(str => str.split('-'))
            .map(inp => inp(0) -> inp(1))
            .foldLeft(Map.empty[String, List[String]])((acc, app) =>
                acc +
                    (app._1 -> (List(app._2) ++ acc.getOrElse(app._1, List.empty[String]))) +
                    (app._2 -> (List(app._1) ++ acc.getOrElse(app._2, List.empty[String])))
            )
            .map((pair) => pair._1 -> pair._2.filter(_ != "start"))

    }

    def partOne(input: Day12Type): String = {
        pathFinder(input, "start", Set.empty[String], Option("neVER!!!!")).toString
    }
    def partTwo(input: Day12Type): String = {
        pathFinder(input, "start", Set.empty[String], Option.empty[String]).toString
    }

    private def pathFinder(state: Day12Type, node: String, seen: Set[String], seenTwice: Option[String] = Option.empty): Int = {
        if (node == "end") {
            1
        }
        else if ((node.toLowerCase == node) && seen.contains(node) && (node == "start" || seenTwice.isDefined)) {
            0
        }
        else {
            val nextSeenTwice = if (seenTwice.isDefined) { seenTwice }
                                else if ((node.toLowerCase == node) && seen.contains(node)) { Option(node) }
                                else { Option.empty }
            val nextSeen = if (node.toLowerCase == node) {seen + node} else {seen}
            state.get(node)
                .getOrElse(List.empty)
                .map(newNode => pathFinder(state, newNode, nextSeen, nextSeenTwice))
                .sum
        }
    }

    private def laughAtBadRecursiveFunction(numTimes: Int)(
        state: Day12Type,
        pathsExploring: Iterable[List[String]],
        pathsVisted: Set[List[String]]
    ): (Day12Type, Iterable[List[String]], Set[List[String]]) = {
        val currPath                      = pathsExploring.head
        val remainPaths                   = pathsExploring.toSet - currPath
        val newVisited: Set[List[String]] = pathsVisted + currPath
        val newPaths: Set[List[String]]   = if (currPath.last == "end") {
            remainPaths
        }
        else {
            val nextSteps: List[List[String]] = state
                .getOrElse(currPath.last, List.empty[String])
                .flatMap(nm => (remainPaths + (currPath ::: nm :: Nil)))
            (remainPaths ++ nextSteps)
                .filter(!pathsVisted.contains(_))
                .filter(path => {
                    val ids = path
                        .groupBy(nodeName => nodeName)
                        .filter((pair: (String, List[String])) => pair._1.toLowerCase.equals(pair._1))
                    ids.filter(p => p._2.size > 1).size < numTimes && ids.filter(p => p._2.size > 2).size < 1
                })
        }
        val finalPaths                    = newPaths.filter(f => f.last != "end")
        val finalVisited                  = newVisited ++ newPaths.filter(p => p.last == "end")
        (state, finalPaths, finalVisited)
    }
}
