import scala.io.Source
import lib.DayTemplate;

object Day01 extends DayTemplate[List[Int]] {

    def parseInput(test: Boolean = false): List[Int] = {
        Source
            .fromResource(
              if (!test) { "day1.txt" }
              else { "day1-test.txt" }
            )
            .getLines
            .toList
            .map(s => s.toInt)
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
