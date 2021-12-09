import scala.io.Source
import lib.DayTemplate
import lib.SubMovement
import lib.ForwardSubMovement
import lib.UpSubMovement
import lib.DownSubMovement
import lib.Coords

object Day02 extends DayTemplate[List[SubMovement]] {

    def parseInput(): List[SubMovement] = {
        Source
            .fromResource("day2.txt")
            .getLines
            .toList
            .map((s: String) => {
                if (s.startsWith("forward")) {
                    new ForwardSubMovement(s.substring(8).toInt)
                } else if (s.startsWith("up")) {
                    new UpSubMovement(s.substring(3).toInt)
                } else {
                    new DownSubMovement(s.substring(5).toInt)
                }
            })
    }

    def partOne(input: List[SubMovement]): String = {
        val finalCoord: Coords =
            input.foldLeft(new Coords(0, 0))((coord, movement) =>
                movement(coord)
            )
        (finalCoord.x * finalCoord.y).toString
    }

    def partTwo(input: List[SubMovement]): String = {
        val finalCoord: Coords = input
            .foldLeft[(Coords, Int)]((new Coords(0, 0), 0))(
              (posn, movement) => {
                  movement match {
                      case UpSubMovement(x: Int)      => (posn._1, posn._2 - x)
                      case DownSubMovement(x: Int)    => (posn._1, posn._2 + x)
                      case ForwardSubMovement(x: Int) =>
                          (
                            movement(posn._1) + new Coords(0, x * posn._2),
                            posn._2
                          )
                      case _ => throw new Exception("Unknown case")
                  }
              }
            )
            ._1
        (finalCoord.x * finalCoord.y).toString
    }
}
