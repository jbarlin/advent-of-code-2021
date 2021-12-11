import lib.DayTemplate
import lib.RecursiveUtils
import lib.Coords
import scala.io.Source
import scala.annotation.tailrec

object Day11 extends DayTemplate[Map[Coords, Int]] {
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

    def addOne(input: Map[Coords, Int]): Map[Coords, Int] = {
        input.map(p => p._1 -> (p._2 + 1))
    }

    def performFlashesStep(input: (Map[Coords, Int], Set[Coords])): (Map[Coords, Int], Set[Coords]) = {
        val state = input._1
        val flashed = input._2
        state
            .filter(c => c._2 > 9)
            .filterNot(c => flashed.contains(c._1))
            .map(c => c._1)
            .foldLeft((state, flashed))((startingState, currFlashing) => {
                val cMap       = startingState._1
                val neighbours =
                    currFlashing.aroundWithDiag().filterNot(startingState._2.contains(_)).filter(cMap.contains(_))
                val nMap       = cMap.filter(p => p._1 != currFlashing && !neighbours.contains(p._1)) ++
                    neighbours.map(p => p -> cMap.get(p).map(_ + 1).get) +
                    (currFlashing -> 0)
                (nMap, startingState._2 + currFlashing)
            })
    }

    def flattenByFlashed(input: Map[Coords, Int], flashed: Set[Coords]): Map[Coords, Int] = {
        input.map(p =>
            p._1 -> (if (flashed.contains(p._1)) { 0 }
                     else { p._2 })
        )
    }

    def partOne(input: Map[Coords, Int]): String = {
        (1 to 100)
            .foldLeft((input, 0))((cState, _) => {
                val currMap         = addOne(cState._1)
                val currCount       = cState._2
                //Move this RepeatUntilNoChanges into a library lol
                val afterAllFlashes = RecursiveUtils.repeatUntilNoChanges((currMap, Set.empty), performFlashesStep);
                val flatten         = flattenByFlashed(afterAllFlashes._1, afterAllFlashes._2)
                (flatten, currCount + afterAllFlashes._2.size)
            })
            ._2
            .toString
    }

    def partTwo(input: Map[Coords, Int]): String = {
        val op = (inp: (Map[Coords, Int], Set[Coords])) => {
            val currMap = addOne(inp._1)
            val afterAllFlashes = RecursiveUtils.repeatUntilNoChanges((currMap, Set.empty), performFlashesStep);
            val flatten         = flattenByFlashed(afterAllFlashes._1, afterAllFlashes._2)
            (flatten, afterAllFlashes._2)
        }
        val prepForNext = (inp: (Map[Coords, Int], Set[Coords])) => {
            (inp._1, Set.empty[Coords])
        }
        val condition = (inp: (Map[Coords, Int], Set[Coords])) => {
            inp._1.size == inp._2.size
        }
        RecursiveUtils
            .countUntilConditionWithPrep((input, Set.empty), op, condition, prepForNext)
            .toString
    }
}
