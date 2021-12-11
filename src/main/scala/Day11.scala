import lib.{Coords, DayTemplate, RecursiveUtils}

import scala.annotation.tailrec
import scala.io.Source

final object Day11 extends DayTemplate[Map[Coords, Int]] {
    def parseInput(): Map[Coords, Int] = {
        Source
            .fromResource("day11.txt")
            .getLines
            .filter(!_.isBlank)
            .zipWithIndex
            .flatMap(line => {
                val lineNo: Int      = line._2
                val lineText: String = line._1
                val coodsBuilder     = Coords.byY(lineNo);
                lineText.toList.zipWithIndex
                    .map(digits => coodsBuilder(digits._2) -> digits._1.toString.toInt)
            })
            .toMap
    }

    def partOne(input: Map[Coords, Int]): String = {
        (1 to 100)
            .foldLeft((input, 0))((cState, _) => {
                val afterFlashOperations = performFlashingOperations((cState._1, Set.empty))
                (afterFlashOperations._1, cState._2 + afterFlashOperations._2.size)
            })
            ._2
            .toString
    }

    def partTwo(input: Map[Coords, Int]): String = {
        val sz          = input.size
        val prepForNext = (inp: (Map[Coords, Int], Set[Coords])) => {
            (inp._1, Set.empty[Coords])
        }
        val condition   = (inp: (Map[Coords, Int], Set[Coords]), count: Int) => {
            sz == inp._2.size
        }
        RecursiveUtils
            .countUntilConditionWithPrep((input, Set.empty), performFlashingOperations, condition, prepForNext)
            .toString
    }

    private def performFlashingOperations(inp: (Map[Coords, Int], Set[Coords])) = {
        val currMap         = inp._1.map(p => p._1 -> (p._2 + 1))
        val afterAllFlashes = RecursiveUtils.repeatUntilNoChanges((currMap, Set.empty), performFlashesStep);
        val flatten         = flattenByFlashed(afterAllFlashes._1, afterAllFlashes._2)
        (flatten, afterAllFlashes._2)
    }

    private def applyFlashies(
        stateMap: Map[Coords, Int],
        remainingToApply: Iterable[Coords],
        flashed: Set[Coords]
    ): (Map[Coords, Int], Iterable[Coords], Set[Coords]) = {
        val currFlashing    = remainingToApply.head
        val validNeighbours = currFlashing.aroundWithDiag
            .filter(!flashed.contains(_))
            .filter(stateMap.contains(_))
        val neighbourSubmap = validNeighbours.map(neighbour => neighbour -> stateMap.get(neighbour).map(_ + 1).get)
        val nextMap         = stateMap ++ neighbourSubmap + (currFlashing -> 0)
        val nextFlashed     = flashed + currFlashing
        (nextMap, remainingToApply.tail, nextFlashed)

    }

    private def performFlashesStep(input: (Map[Coords, Int], Set[Coords])): (Map[Coords, Int], Set[Coords]) = {
        val state       = input._1
        val flashed     = input._2
        val needToFlash = state
            .filter(c => c._2 > 9)
            .filterNot(c => flashed.contains(c._1))
            .map(c => c._1)
        RecursiveUtils.applyStackOperationsToMap(state, needToFlash, applyFlashies, flashed)
    }

    private def flattenByFlashed(input: Map[Coords, Int], flashed: Set[Coords]): Map[Coords, Int] = {
        input.map(p =>
            p._1 -> (
              if (flashed.contains(p._1)) {
                  0
              }
              else {
                  p._2
              }
            )
        )
    }
}
