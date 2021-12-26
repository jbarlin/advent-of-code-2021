import lib.DayTemplate
import lib.Coords
import lib.pathing.{WeightedMap, WeightedPath}

import scala.io.Source

final case class D15Inp(val smallerMap: WeightedMap[Coords] , val largerMap: WeightedMap[Coords], val maxCoord: Coords)

object Day15 extends DayTemplate[D15Inp] {
    def parseInput(): D15Inp = {
        val coordsAndNum = Source
            .fromResource("day15-test.txt")
            .getLines
            .filter(!_.isBlank)
            .zipWithIndex
            .toList
            .flatMap((b) => {
                def fromY = Coords.byY(b._2);
                b._1.toList.zipWithIndex
                    .map(c => (fromY(c._2), c._1.toInt - 48))
            })
        val coords       = coordsAndNum.map(_._1).toSet
        val maxCoord     = coords.maxBy(b => (b.x, b.y))
        val oMap = coordsAndNum
                            .foldLeft(new WeightedMap[Coords](Map.empty))
                            ((weightedMap, pairing) => {
                                val myCoord: Coords = pairing._1
                                val tCost           = pairing._2
                                val myCost          = if (tCost > 9) { (9 - tCost).abs} else {tCost}
                                val byTo = WeightedPath.byTo(myCoord, myCost)
                                    myCoord.aroundNoDiag
                                        .foldLeft(weightedMap)((a, b) => a.addPath(byTo(b)))

                            })
        val tMap = (0 until 5)
            .foldLeft(new WeightedMap[Coords](Map.empty))((xAcc, xMod) => {
                (0 until 5)
                    .foldLeft(xAcc)((acc, yMod) => {
                        coordsAndNum
                            .foldLeft(acc)((weightedMap, pairing) => {
                                val myCoord: Coords = pairing._1.addX(maxCoord.x * xMod).addY(maxCoord.y * yMod)
                                val tCost           = pairing._2 + xMod + yMod
                                val myCost          = if tCost % 9 == 0 then 9 else tCost % 9
                                val byTo = WeightedPath.byTo(myCoord, myCost)
                                    myCoord.aroundNoDiag
                                        .foldLeft(weightedMap)((a, b) => a.addPath(byTo(b)))

                            })
                    })
            })
        new D15Inp(oMap, tMap, maxCoord)
    }

    def partOne(input: D15Inp): String = {
        val mySolved = input.smallerMap
            .pathBetween(new Coords(0, 0), input.maxCoord)
        mySolved
            .getOrElse(0.0)
            .toLong
            .toString
    }
    def partTwo(input: D15Inp): String = {
        val myMax = new Coords(input.maxCoord.x * 5, input.maxCoord.y * 5)
        val mySolved = input.largerMap
            .pathBetween(new Coords(0, 0), myMax)
        mySolved
            .getOrElse(0.0)
            .toLong
            .toString
    }
}
