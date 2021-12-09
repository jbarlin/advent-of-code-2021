import lib.DayTemplate
import lib.Coords
import scala.io.Source
import scala.annotation.tailrec

object Day9 extends DayTemplate[Map[Coords, Int]] {
    def parseInput(): Map[Coords, Int] = {
        Source
            .fromResource("day9.txt")
            .getLines
            .zipWithIndex
            .flatMap(line => {
                val lineNo: Int = line._2
                val lineText: String = line._1
                val coodsBuilder = Coords.byY(lineNo);
                lineText.toList
                    .zipWithIndex
                    .map(digits => {
                        val num: Int = digits._1.toString.toInt
                        val indx = digits._2
                        coodsBuilder(indx) -> num
                    })
            })
            .toMap
    }

    def partOne(input: Map[Coords, Int]): String = {
        input
            .map(part => {
                val list = part._1.aroundMe((0,0), (Int.MaxValue, Int.MaxValue), false)
                (list, part._2)
            })
            .filter(part => {
                part._1
                .forall(
                    c => input.get(c).getOrElse(Int.MaxValue) > part._2
                )
            })
            .map(_._2+1)
            .sum
            .toString
    }
    @tailrec
    def findAllUntilNine(originalMap: Map[Coords, Int], checking: Set[Coords], checked: Set[Coords]): Set[Coords] = {
        if (checking.isEmpty){
            checked
        }else{
            val top = checking.head
            val remaining = checking - top
            val cSet = if (originalMap.get(top).getOrElse(9) < 9) {
                    Set(top)
                }else{
                    Set.empty
                }
            findAllUntilNine(originalMap, remaining ++ (cSet.flatMap(c => c.aroundMe())) -- checked, checked ++ cSet )
        }
    }

    def partTwo(input: Map[Coords, Int]): String = {
        val smallest: List[Coords] = input
            .map(part => {
                val list = part._1.aroundMe((0,0), (Int.MaxValue, Int.MaxValue), false)
                ((list, part._1), part._2)
            })
            .filter(part => {
                part._1._1
                .forall(
                    c => input.get(c).getOrElse(Int.MaxValue) > part._2
                )
            })
            .map(_._1._2)
            .toList
        smallest
            .map(coord => {
                findAllUntilNine(input, coord.aroundMe().toSet, Set(coord)).size
            })
            .sortWith((a, b) => a > b)
            .take(3)
            .reduce((a, b) => a * b)
            .toString
    }
}
