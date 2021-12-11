import lib.{Coords, DayTemplate}

import scala.annotation.tailrec
import scala.io.Source

final object Day09 extends DayTemplate[Map[Coords, Int]] {
    def parseInput(): Map[Coords, Int] = {
        Source
            .fromResource("day9.txt")
            .getLines
            .zipWithIndex
            .flatMap(line => {
                val lineNo: Int = line._2
                val lineText: String = line._1
                val coodsBuilder = Coords.byY(lineNo);
                lineText.toList
                    .zipWithIndex
                    .map(digits => {
                        val num: Int = digits._1.toString.toInt
                        val indx = digits._2
                        coodsBuilder(indx) -> num
                    })
            })
            .toMap
    }

    def partOne(input: Map[Coords, Int]): String = {
        input
            .map(part => {
                val list = part._1.aroundMe((0, 0), (Int.MaxValue, Int.MaxValue), false)
                (list, part._2)
            })
            .filter(part => {
                part._1
                    .forall(
                        c => input.get(c).getOrElse(Int.MaxValue) > part._2
                    )
            })
            .map(_._2 + 1)
            .sum
            .toString
    }

    def partTwo(input: Map[Coords, Int]): String = {
        input
            .map(part => {
                val list = part._1.aroundMe((0, 0), (Int.MaxValue, Int.MaxValue), false)
                ((list, part._1), part._2)
            })
            .filter(part => {
                part._1._1
                    .forall(
                        c => input.get(c).getOrElse(Int.MaxValue) > part._2
                    )
            })
            .map(_._1._2)
            .toList
            .map(coord => {
                findAllUntilNine(input, coord.aroundMe().toSet, Set(coord)).size
            })
            .sortWith((a, b) => a > b)
            .take(3)
            .reduce((a, b) => a * b)
            .toString
    }

    @tailrec
    private def findAllUntilNine(originalMap: Map[Coords, Int], checking: Set[Coords], checked: Set[Coords]): Set[Coords] = {
        if (checking.isEmpty) {
            checked
        } else {
            val top = checking.head
            val remaining = checking - top
            val cSet = if (originalMap.get(top).getOrElse(9) < 9) {
                Set(top)
            } else {
                Set.empty
            }
            findAllUntilNine(originalMap, remaining ++ (cSet.flatMap(c => c.aroundMe())) -- checked, checked ++ cSet)
        }
    }
}
