import lib.DayTemplate

import scala.annotation.tailrec
import scala.io.Source

final object Day07 extends DayTemplate[List[Int]] {
    def parseInput(): List[Int] = {
        Source
            .fromResource("day7.txt")
            .getLines
            .filter(p => p.length > 0)
            .flatMap(p => p.split(","))
            .map(s => s.toInt)
            .toList
    }

    def partOne(input: List[Int]): String = {
        computeLowestCost(Int.MaxValue, 0, input.max, input, (a, b) => (a - b).abs)
            .toString
    }

    def partTwo(input: List[Int]): String = {
        computeLowestCost(Int.MaxValue, 0, input.max, input, costToMove)
            .toString
    }

    @tailrec
    private def computeLowestCost(min: Int, curr: Int, max: Int, input: List[Int], method: (Int, Int) => Int): Int = {
        val thisStepDiff = input.map(a => method(a, curr)).sum
        val minSoFar = Math.min(min, thisStepDiff)
        if (curr == max) {
            minSoFar
        } else {
            computeLowestCost(minSoFar, curr + 1, max, input, method)
        }
    }

    private def costToMove(from: Int, to: Int): Int = {
        val start = Math.min(from, to)
        val end = Math.max(from, to)
        ((end - start) + (end - start - 1)) / 2
    }
}

// 352
