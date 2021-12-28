import lib.Coords
import lib.DayTemplate
import scala.io.Source

private def parseBin(in: Seq[Int]): Long = {
    java.lang.Long.parseLong(in.foldLeft("")((a, b) => a + b.toString), 2)
}

//Am I burning RAM storing the map? Yes
//Do I care? No

private final case class Day20(val translationField: Seq[Int], val definedPlane: Map[Coords, Int], val voidState: Int = 0) {

    def enhance: Day20 = {
        val maxCoord                     = definedPlane.keySet.maxBy(c => (c.y, c.x))
        val maxComputeCoord              = maxCoord.addX(1).addY(1)
        val minComputeCoord              = Coords(-1)(-1)
        def getFromPlane(c: Coords): Int = {
            this.definedPlane.getOrElse(c, voidState)
        }
        val nPlane = minComputeCoord
            .to(maxComputeCoord)
            .foldLeft(Map.empty[Coords, Int])((acc, currCoord) => {
                val myAround: List[Coords]      = currCoord
                    .addX(-1)
                    .addY(-1)
                    .to(currCoord.addX(1).addY(1))
                    .sortBy(c => (c.y, c.x))
                    .toList
                val myNums        = myAround
                    .map(getFromPlane)
                val myBin         = parseBin(myNums).toInt
                if (myBin < 0 || myBin > 512) {
                    throw new IllegalStateException("Huh?");
                }
                val myTranslation = translationField(myBin)
                if (myTranslation > 1 || myTranslation < 0){
                    throw new IllegalStateException("WTF!")
                }
                acc + (currCoord.addX(1).addY(1) -> myTranslation)
            })
        val nVoidState = if (voidState == 0) {
            translationField(0)
        }else{
            translationField(511)
        }
        new Day20(translationField, nPlane, nVoidState)
    }

    lazy val count = this.definedPlane.count(_._2 == 1)

}

object Day20 extends DayTemplate[Day20] {
    def parseInput(test: Boolean): Day20 = {
        val textInput        = Source
            .fromResource(
              if (!test) { "day20.txt" }
              else { "day20-test.txt" }
            )
            .getLines
        val fLineIn          = textInput.next
        val translationField = fLineIn.map {
            case '#' => 1
            case _   => 0
        }.toSeq
        val plane            = textInput
            .filter(!_.isBlank)
            .zipWithIndex
            .foldLeft(Map.empty[Coords, Int])((acc, posn) => {
                val line       = posn._1
                val linNum     = posn._2
                val coordMaker = Coords.byY(linNum)
                line.zipWithIndex
                    .foldLeft(acc)((xAcc, curr) => {
                        val char = curr._1
                        val x    = curr._2
                        val chValue = char match {
                            case '#' => 1
                            case _   => 0
                        }
                        val coord = coordMaker(x)
                        xAcc + (coord -> chValue)
                    })
            })
        
        new Day20(translationField, plane)
    }

    def partOne(input: Day20): String = {
        val enhA = input.enhance
        val enhB = enhA.enhance
        enhB.count.toString
        //5575 is too high?
        //Oh - the void blinks!
    }
    def partTwo(input: Day20): String = {
        Iterator.iterate(input)(_.enhance).drop(50).next().count.toString
    }

}
