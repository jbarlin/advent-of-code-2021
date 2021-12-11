import lib.DayTemplate

import scala.annotation.tailrec
import scala.io.Source

final object Day06 extends DayTemplate[Map[Int, Long]] {
    def parseInput(): Map[Int, Long] = {
        Source
            .fromResource("day6.txt")
            .getLines
            .filter(p => p.length > 0)
            .flatMap(p => p.split(","))
            .map(s => s.toInt)
            .toList
            .groupBy(identity)
            .mapValues(_.size)
            .toMap
            .map((p) => (p._1, p._2.toLong))
    }

    def partOne(input: Map[Int, Long]): String = {
        iterate(input, 80).toString
    }

    def partTwo(input: Map[Int, Long]): String = {
        iterate(input, 256).toString
    }

    @tailrec
    private def iterate(input: Map[Int, Long], count: Int): Long = {
        val nMap = input
            .foldLeft(Map.empty[Int, Long])((acc, pairToAdd) => {
                val (count, name) = (pairToAdd._2, pairToAdd._1);
                if (name == 0) {
                    acc + ((6, count + acc.getOrElse(6, 0L))) + ((8, count))
                }
                else if (name == 7) {
                    acc + ((6, count + acc.getOrElse(6, 0L)))
                }
                else {
                    acc + ((name - 1, count))
                }
            })
        if (count == 1) {
            nMap.map(_._2).sum
        }
        else {
            iterate(nMap, count - 1)
        }

    }
}
