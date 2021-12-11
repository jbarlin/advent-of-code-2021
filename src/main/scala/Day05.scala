import lib.day5.VentPath
import lib.{Coords, DayTemplate, LinearVector}

import scala.annotation.tailrec
import scala.io.Source

final object Day05 extends DayTemplate[List[VentPath]] {
    def parseInput(): List[VentPath] = {
        Source
            .fromResource("day5.txt")
            .getLines
            .filter(p => p.length > 0)
            .map(str => str.split(" -> ").flatMap(s => s.split(",")).map(s => s.toInt))
            .map(ln => VentPath(new Coords(ln(0), ln(1)))(new Coords(ln(2), ln(3))))
            .toList
    }

    def partOne(input: List[VentPath]): String = {
        input
            .filter(p => !p.isDiagonal)
            .foldLeft(Map.empty[Coords, Int])((acc, app) => app.apply(acc))
            .count(v => v._2 > 1)
            .toString
    }

    def partTwo(input: List[VentPath]): String = {
        input
            .foldLeft(Map.empty[Coords, Int])((acc, app) => app.apply(acc))
            .count(v => v._2 > 1)
            .toString
    }
}
