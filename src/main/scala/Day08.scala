import lib.DayTemplate
import lib.day8.NoteEntry
import scala.collection.parallel.CollectionConverters._
import scala.io.Source
import scala.collection.parallel.immutable.ParSeq

object Day08 extends DayTemplate[ParSeq[NoteEntry]] {
    val uniqueNumbers: Set[Int] = (2 :: 3 :: 4 :: 7 :: Nil).toSet

    def parseInput(): ParSeq[NoteEntry] = {
        Source
            .fromResource("day8.txt")
            .getLines
            .toSeq
            .par
            .filter(p => p.length > 0)
            .map[Array[String]](p => p.split('|'))
            .map(p => {
                (p.head.trim(), p.tail.head.trim())
            })
            .map(p =>
                new NoteEntry(
                    p._1
                        .split(" ")
                        .toList,
                    p._2
                        .split(" ")
                        .toList
                )
            )
    }

    def partOne(input: ParSeq[NoteEntry]): String = {
        input
            .map(m => {
                m.outputValues.count(c => uniqueNumbers.contains(c.size))
            })
            .sum
            .toString
    }

    def partTwo(input: ParSeq[NoteEntry]): String = {
        input.map(_.apply).sum.toString
    }
}
