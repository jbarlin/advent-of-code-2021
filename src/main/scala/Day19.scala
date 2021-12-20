import lib.DayTemplate
import lib.day19.{Coord3, Scanner}
import org.apache.commons.lang3.builder.HashCodeBuilder
import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.immutable.ParSet
import scala.io.Source
import scala.collection.parallel.ParIterable
import scala.collection.View

type Day19Type = (Set[Coord3], Map[Int, Coord3])


object Day19 extends DayTemplate[Day19Type] {
    final def parseInput(): Day19Type = {
        val fnlInp: Seq[Scanner] = Scanner.apply("day19.txt")
        Scanner.solve(fnlInp)
    }

    final def partOne(input: Day19Type): String = input._1.size.toString

    final def partTwo(input: Day19Type): String = input._2.values
        .toSeq
        .combinations(2)
        .map({ case Seq(a, b) => a.distance(b) })
        .max
        .toString
}
