import lib.DayTemplate
import lib.Coords
import lib.pathing.{WeightedMap, WeightedPath}

import scala.io.Source

type D15 = (Array[Array[Int]], Array[Array[Int]]);

object Day15 extends DayTemplate[D15] {
    def parseInput(test: Boolean): D15 = {
        val smallerMap: Array[Array[Int]] = Source
            .fromResource(
              if (true || !test) { "day15.txt" }
              else { "day15-test.txt" }
            )
            .getLines
            .filter(!_.isBlank)
            .toArray
            .map(ln => 
                ln.toArray.map(c => c.toInt - 48)
            )
        val largestSX = smallerMap.size
        val largestSY = smallerMap.head.size
        val largerMap: Array[Array[Int]] = Array.ofDim((largestSY) * 6, (largestSX) * 6)
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
                                    val fnlX = xIndx + (largestSX * xMod)
                                    val fnlY = yIndx + (largestSY * yMod)
                                    val fnlNum = ((num + xMod + yMod - 1) % 9) + 1
                                    largerMap(fnlY).update(fnlX, fnlNum)
                                })
                            })
                    })
            });
        (smallerMap, largerMap)
    }

    def partOne(input: D15): String = {
        val myInput = input._1
        ???
    }
    def partTwo(input: D15): String = {
        val myInput = input._2
        ???
    }
}
