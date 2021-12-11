import scala.io.Source
import lib.DayTemplate;

final object Day01 extends DayTemplate[List[Int]] {
    
    def parseInput(): List[Int] = {
        Source.fromResource("day1.txt").getLines.toList.map(s => s.toInt)
    }

    def partOne(dayOneInput: List[Int]): String = {
        dayOneInput
            .zip(dayOneInput.tail)
            .count(pair => pair._2 > pair._1)
            .toString
    }

    def partTwo(dayOneInput: List[Int]): String = {
        dayOneInput
            .zip(dayOneInput.tail.tail.tail)
            .count(pair => pair._2 > pair._1)
            .toString
    }
}
