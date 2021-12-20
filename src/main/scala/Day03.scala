import lib.DayTemplate
import lib.day3.{BinarySearcher, Tally}

import scala.io.Source
import scala.math
import scala.annotation.tailrec

object Day03 extends DayTemplate[List[List[Int]]] {
    def parseInput() = {
        Source
            .fromResource("day3.txt")
            .getLines
            .map(str => {
                str.toList
                    .map((c) => {
                        c.toInt - 48
                    })
            })
            .toList
    }

    def partOne(input: List[List[Int]]): String = {
        val startTally: List[Tally] = input.head.map(_ => new Tally());

        val binary: (String, String) = input
              .map((list) => {
                  list
                      .map(dig => new Tally().add(dig))
              })
              .fold(startTally)((tallyA, tallyB) => {
                  tallyA
                      .zip(tallyB)
                      .map(pair => pair._1 + pair._2)
              })
            .foldLeft(("", ""))((strs, tally) => {
                (
                  strs._1 + tally.toLarger().toString,
                  strs._2 + tally.toSmaller().toString
                )
            });

        (Integer.parseInt(binary._1, 2) * Integer.parseInt(binary._2, 2))
            .toString()
    }

    def partTwo(input: List[List[Int]]): String = {
        val binSearch: BinarySearcher = new BinarySearcher(input, input);
        val searched: BinarySearcher  = binSearch.reduce(0);
        val larger: String            = searched.larger.head.foldLeft("")((c, u) => c + u.toString)
        val smaller: String           = searched.smaller.head.foldLeft("")((c, u) => c + u.toString)
        (Integer.parseInt(larger, 2) * Integer.parseInt(smaller, 2))
            .toString()
    }
}
