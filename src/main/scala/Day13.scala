import lib.DayTemplate
import lib.Coords
import scala.io.Source
import scala.annotation.tailrec

type coordsMap = Set[Coords]
type opTuple = (Option[Int], Option[Int])
type Day13Type = (coordsMap, List[opTuple])

object Day13 extends DayTemplate[Day13Type] {
    def parseInput(): Day13Type = {
        val inp = Source
            .fromResource("day13.txt")
            .getLines
            .zipWithIndex
            .toList
        val pt = inp.filter(p => p._1.isBlank).head._2
        val (coordsList, foldAlong) = inp.filter(!_._1.isBlank).partition(_._2 < pt)
        val mp: coordsMap = coordsList
            .map(p => p._1.split(','))
            .map(p => Coords(p(0).toInt)(p(1).toInt))
            .toSet
        val offsets = foldAlong.map(p => p._1.split('='))
                        .map(split => {
                            if (split(0).endsWith("x")){
                                (Option(split(1).toInt), Option.empty[Int])
                            }else{
                                (Option.empty[Int], Option(split(1).toInt))
                            }
                        })
        (mp, offsets)
    }

    @tailrec
    private def applyFolds(originalMap: Set[Coords], folds: List[opTuple]): Set[Coords] = {
        if (folds.isEmpty) {
            originalMap
        } else {
            val offset = folds.head
            val mirrored = originalMap.map(c => c.foldAround(offset))
            val originalKeysAndMirrored = originalMap ++ mirrored
            val afterMirror = originalKeysAndMirrored
                .filter(c => c.x < offset._1.getOrElse(Int.MaxValue))
                .filter(c => c.y < offset._2.getOrElse(Int.MaxValue))
                .filter(c => c.x >= 0 && c.y >= 0)
            applyFolds(afterMirror, folds.tail)
        }
    }

    def partOne(input: Day13Type): String = {
        applyFolds(input._1, List(input._2.head)).size.toString
    }

    def partTwo(input: Day13Type): String = {
        val foldsApplied = applyFolds(input._1, input._2)
        "\n\n" + Coords.mapToString(foldsApplied) + "\n\n"
    }
}