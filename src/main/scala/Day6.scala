import scala.io.Source
import lib.DayTemplate
import scala.annotation.tailrec
object Day6 extends DayTemplate[Map[Int, Long]] {
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

    def convertMap(input: Map[Int, Long]): Map[Int, Long] = {
        input
            .foldLeft(Map.empty[Int, Long])((acc, pairToAdd) => {
                val count = pairToAdd._2;
                val name  = pairToAdd._1
                if (name == 0) {
                    acc + ((6, count + acc.getOrElse(6, 0L))) + ((8, count))
                } else if (name == 7)
                {
                    acc + ((6, count + acc.getOrElse(6, 0L)))
                }
                else {
                    acc + ((name - 1, count))
                }
            })
    }

    @tailrec
    def iterate(input: Map[Int, Long], count: Int): Map[Int, Long] = {
        val nMap = convertMap(input)
        if (count == 1) {
            nMap
        }
        else {
            iterate(nMap, count - 1)
        }

    }

    def partOne(input: Map[Int, Long]): String = {
        iterate(input, 80).map(_._2).sum.toString
    }

    def partTwo(input: Map[Int, Long]): String = {
        iterate(input, 256).map(_._2).sum.toString
    }
}
