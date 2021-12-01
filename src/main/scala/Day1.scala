import scala.io.Source
import scala.io.Source

object Day1 {
    val dayOneInput: List[Int] = Source.fromResource("day1.txt").getLines.toList.map(s => s.toInt)
    def partOne(): String = {
       val pairs = dayOneInput.zip(dayOneInput.tail)
       pairs.foldLeft(0)((accum, pair) => accum + (if (pair._2 > pair._1) {1} else {0})).toString
    }
    def partTwo(): String = {
        val triples = (dayOneInput.zip(dayOneInput.tail)).zip(dayOneInput.tail.tail)
        val flatten = triples.map(mess => mess._1._1 + mess._1._2 + mess._2).toList
        (flatten.zip(flatten.tail)).foldLeft(0)((accum, pair) => accum + (if (pair._2 > pair._1) {1} else {0})).toString
    }
}
