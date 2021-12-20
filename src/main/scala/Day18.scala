import lib.DayTemplate
import lib.day18.SnailNumber

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

type Day18Type = Seq[SnailNumber]

object Day18 extends DayTemplate[Day18Type] {
    def parseInput(): Day18Type = {
        Source
            .fromResource("day18.txt")
            .getLines
            .filter(!_.isBlank)
            .map(
              SnailNumber(_)._2
            )
            .toSeq
    }

    def partOne(input: Day18Type): String = {
        input.reduce(SnailNumber.add).magnitude.toString
    }

    def partTwo(input: Day18Type): String = {
        input
            .combinations(2)
            .toSeq
            .par
            .flatMap[(SnailNumber, SnailNumber)](s => (s.head, s.reverse.head) :: (s.reverse.head, s.head) :: Nil)
            .map(SnailNumber.add(_, _).magnitude)
            .max
            .toString
    }
}
