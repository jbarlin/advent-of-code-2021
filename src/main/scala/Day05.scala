import lib.day5.VentPath
import lib.{Coords, DayTemplate, LinearVector}

import scala.annotation.tailrec
import scala.io.Source
import lib.RecursiveUtils

object Day05 extends DayTemplate[List[VentPath]] {
    def parseInput(test: Boolean = false): List[VentPath] = {
        Source
            .fromResource(
              if (!test) { "day5.txt" }
              else { "day5-test.txt" }
            )
            .getLines
            .filter(p => p.length > 0)
            .map(str => str.split(" -> ").flatMap(s => s.split(",")).map(s => s.toInt))
            .map(ln => VentPath(new Coords(ln(0), ln(1)))(new Coords(ln(2), ln(3))))
            .toList
    }

    def partOne(input: List[VentPath]): String = {
        partApply(input.filter(p => !p.isDiagonal))
    }

    def partTwo(input: List[VentPath]): String = {
        partApply(input)
    }

    private def stackOp(state: Map[Coords, Int], left: Iterable[VentPath], s: Set[VentPath]) = {
        val c = left.head
        (c.apply(state), left.tail, s)
    }

    private def partApply(operateOn: List[VentPath]): String = {
        RecursiveUtils
            .applyStackOperationsToMap(
              Map.empty[Coords, Int],
              operateOn,
              stackOp,
              Set.empty
            )
            ._1
            .count(v => v._2 > 1)
            .toString
    }
}
