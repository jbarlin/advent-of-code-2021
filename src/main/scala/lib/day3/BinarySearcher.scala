package lib.day3

import scala.annotation.tailrec

final class BinarySearcher(val larger: List[List[Int]],
                           val smaller: List[List[Int]]
                          ) {
    @tailrec
    final def reduce(index: Int): BinarySearcher = {
        val largerTally = this.larger.foldLeft(new Tally())((tally, row) => {
            tally.add(row(index))
        });
        val larger = {
            if this.larger.size <= 1 then {
                this.larger
            }
            else {
                this.larger
                    .filter((innerBin) => {
                        innerBin(index) == {
                            if !largerTally.isEqual() then {
                                largerTally.toLarger()
                            }
                            else {
                                1
                            }
                        }
                    })
            }
        }
        
        val smallerTally = this.smaller.foldLeft(new Tally())((tally, row) => {
            tally.add(row(index))
        });
        val smaller = {
            if this.smaller.size <= 1 then {
                this.smaller
            }
            else {
                this.smaller
                    .filter((innerBin) => {
                        innerBin(index) == {
                            if !smallerTally.isEqual() then {
                                smallerTally.toSmaller()
                            }
                            else {
                                0
                            }
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
