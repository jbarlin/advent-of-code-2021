import lib.{Coords, DayTemplate}
import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec
import scala.io.Source
import lib.RecursiveUtils
import scala.collection.parallel.immutable.ParMap

type Awkies = Map[(List[lib.Coords], lib.Coords), Int]

object Day09 extends DayTemplate[ParMap[Coords, Int]] {
    def parseInput(): ParMap[Coords, Int] = {
        Source
            .fromResource("day9.txt")
            .getLines
            .zipWithIndex
            .flatMap(line => {
                val lineNo: Int      = line._2
                val lineText: String = line._1
                val coodsBuilder     = Coords.byY(lineNo);
                lineText.toList.zipWithIndex
                    .map(digits => {
                        val num: Int = digits._1.toString.toInt
                        val indx     = digits._2
                        coodsBuilder(indx) -> num
                    })
            })
            .toMap
            .par
    }

    def partOne(input: ParMap[Coords, Int]): String = {
        input
            .map(part => {
                val list = part._1.aroundNoDiag
                (list, part._2)
            })
            .filter(part => {
                part._1
                    .forall(c => input.get(c).getOrElse(Int.MaxValue) > part._2)
            })
            .map(_._2 + 1)
            .sum
            .toString
    }

    def partTwo(input: ParMap[Coords, Int]): String = {
        input
            .map(part => {
                val list = part._1.aroundNoDiag
                ((list, part._1), part._2)
            })
            .filter(part => {
                part._1._1
                    .forall(c => input.get(c).getOrElse(Int.MaxValue) > part._2)
            })
            .map(_._1._2)
            .toList
            .map(coord => {
                RecursiveUtils.applyStackOperationsToMap(input.seq, coord.aroundNoDiag.toSet, findAllUntilNine, Set(coord))._2.size
            })
            .sortWith((a, b) => a > b)
            .take(3)
            .reduce((a, b) => a * b)
            .toString
    }

    private def findAllUntilNine(
        originalMap: Map[Coords, Int],
        checking: Iterable[Coords],
        checked: Set[Coords]
    ): (Map[Coords, Int], Iterable[Coords], Set[Coords]) = {
        val top       = checking.head
        val remaining = checking.toSet - top
        val cSet      = if (originalMap.get(top).getOrElse(9) < 9) {
            Set(top)
        }
        else {
            Set.empty
        }
        val remainsToCheck = remaining ++ (cSet.flatMap(c => c.aroundNoDiag)) -- checked
        val haveChecked = checked ++ cSet
        (originalMap, remainsToCheck, haveChecked)
    }
}
