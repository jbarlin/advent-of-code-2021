import lib.DayTemplate
import lib.day8.NoteEntry

import scala.io.Source


final object Day08 extends DayTemplate[Iterable[NoteEntry]] {
    val uniqueNumbers: Set[Int] = (2 :: 3 :: 4 :: 7 :: Nil).toSet

    def parseInput(): Iterable[NoteEntry] = {
        Source
            .fromResource("day8.txt")
            .getLines
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
            ).iterator.to(Iterable)
    }

    def partOne(input: Iterable[NoteEntry]): String = {
        input
            .map(m => {
                m.outputValues.count(c => uniqueNumbers.contains(c.size))
            })
            .sum
            .toString
    }

    def partTwo(input: Iterable[NoteEntry]): String = {
        input.map(_.apply).sum.toString
    }
}
