import lib.DayTemplate
import scala.io.Source
import scala.math
import scala.annotation.tailrec

class Tally(val zeros: Int = 0, val ones: Int = 0) {
    def add(digit: Int): Tally = {
        if digit == 1 then {
            Tally(this.zeros, this.ones + 1)
        }
        else {
            Tally(this.zeros + 1, this.ones)
        }
    }

    def +(that: Tally) = {
        Tally(that.zeros + this.zeros, that.ones + this.ones)
    }

    def isEqual(): Boolean = {
        this.ones == this.zeros
    }

    def toLarger(): Int = {
        if this.ones > this.zeros then {
            1
        }
        else {
            0
        }
    }

    def toSmaller(): Int = {
        if this.ones < this.zeros then {
            1
        }
        else {
            0
        }
    }
}

class BinarySearcher(
    val larger: List[List[Int]],
    val smaller: List[List[Int]]
) {
    @tailrec
    final def reduce(index: Int): BinarySearcher = {
        val largerTally  = this.larger.foldLeft(new Tally())((tally, row) => {
            tally.add(row(index))
        });
        val larger       = {
            if this.larger.size <= 1 then { this.larger }
            else {
                this.larger
                    .filter((innerBin) => {
                        innerBin(index) == {
                            if !largerTally.isEqual() then { largerTally.toLarger() }
                            else { 1 }
                        }
                    })
            }
        }
        val smallerTally = this.smaller.foldLeft(new Tally())((tally, row) => {
            tally.add(row(index))
        });
        val smaller      = {
            if this.smaller.size <= 1 then { this.smaller }
            else {
                this.smaller
                    .filter((innerBin) => {
                        innerBin(index) == {
                            if !smallerTally.isEqual() then { smallerTally.toSmaller() }
                            else { 0 }
                        }
                    })
            }
        }
        if larger.size == smaller.size && smaller.size == 1 then {
            new BinarySearcher(larger, smaller)
        }
        else {
            new BinarySearcher(larger, smaller).reduce(index + 1)
        }

    }
}

object Day3 extends DayTemplate[(List[Tally], List[List[Int]])] {
    def parseInput() = {
        val input = Source
            .fromResource("day3.txt")
            .getLines
            .map(str => {
                str.toList
                    .map((c) => {
                        c.toInt - 48
                    })
            })
            .toList

        val startTally: List[Tally] = input.head.map(_ => new Tally());
        (
          input
              .map((list) => {
                  list
                      .map(dig => new Tally().add(dig))
              })
              .fold(startTally)((tallyA, tallyB) => {
                  tallyA
                      .zip(tallyB)
                      .map(pair => pair._1 + pair._2)
              }),
          input
        )
    }

    def partOne(input: (List[Tally], List[List[Int]])): String = {
        val binary: (String, String) = input._1
            .foldLeft(("", ""))((strs, tally) => {
                (
                  strs._1 + tally.toLarger().toString,
                  strs._2 + tally.toSmaller().toString
                )
            });

        (Integer.parseInt(binary._1, 2) * Integer.parseInt(binary._2, 2))
            .toString()
    }

    def partTwo(input: (List[Tally], List[List[Int]])): String = {
        val binaries: List[List[Int]] = input._2
        val binSearch: BinarySearcher = new BinarySearcher(binaries, binaries);
        val searched: BinarySearcher  = binSearch.reduce(0);
        val larger: String            = searched.larger.head.foldLeft("")((c, u) => c + u.toString)
        val smaller: String           = searched.smaller.head.foldLeft("")((c, u) => c + u.toString)
        (Integer.parseInt(larger, 2) * Integer.parseInt(smaller, 2))
            .toString()
    }
}
