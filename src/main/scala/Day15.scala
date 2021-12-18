import lib.DayTemplate
import lib.Coords
import lib.AStar
import lib.pathing.{WeightedMap, WeightedPath}

import scala.io.Source

final case class D15Inp(val smallerMap: AStar , val largerMap: AStar)

object Day15 extends DayTemplate[D15Inp] {
    def parseInput(): D15Inp = {
        val smallerMap: Array[Array[Int]] = Source
            .fromResource("day15.txt")
            .getLines
            .filter(!_.isBlank)
            .toArray
            .map(ln => 
                ln.toArray.map(c => c.toInt - 48)
            )
        val largestX = smallerMap.size
        val largestY = smallerMap.head.size
        val largerMap: Array[Array[Int]] = Array.empty[Array[Int]]
        smallerMap.zipWithIndex
            .foreach(a => {
                val xIndx = a._2
                a._1
                    .zipWithIndex
                    .foreach(b => {
                        val yIndx = b._2
                        val num = b._1
                        (0 to 5)
                            .foreach(xMod => {
                                (0 to 5).foreach(yMod => {
                                    val fnlX = xIndx + (largestX * xMod)
                                    val fnlY = yIndx + (largestY * yMod)
                                    val fnlNum = ((num + xMod + yMod - 1) % 9) + 1
                                    largerMap(fnlX).update(fnlY, fnlNum)
                                })
                            })
                    })
            });
        D15Inp(new AStar(smallerMap), new AStar(largerMap))
    }

    def partOne(input: D15Inp): String = {
        ???
    }
    def partTwo(input: D15Inp): String = {
        ???
    }
}
