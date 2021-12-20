package lib.day19
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

final case class Scanner(beacons: Set[Coord3]) {
    final val orientations: Vector[Set[Coord3]] = beacons.map(_.rotations).transpose.map(_.toSet).toVector

    final override def toString: String = "Scanner(" + beacons + ")"
}

object Scanner {

    final def solve(scanners: Seq[Scanner]): (Set[Coord3], Map[Int, Coord3]) = {
        val possibles = scanners.zipWithIndex
        innerSolve(possibles.tail, scanners.head.beacons, Map(0 -> Coord3(0, 0, 0)))
    }

    @tailrec
    final private def innerSolve(
        scanners: Seq[(Scanner, Int)],
        beacons: Set[Coord3],
        scanMap: Map[Int, Coord3]
    ): (Set[Coord3], Map[Int, Coord3]) = {
        if (scanners.isEmpty) {
            (beacons, scanMap)
        }
        else {
            val relative: Seq[(Set[Coord3], Coord3, Int)] = scanners.view
                .map(sc => {
                    sc._1.orientations.par
                        .flatMap(rotated => {
                            beacons.view.flatMap(beaconFrom1 => {
                                rotated.view
                                    .map(beaconFrom2 => (rotated, beaconFrom1, beaconFrom2))
                            })
                        })
                        .map(triple => {
                            val diff           = triple._2 - triple._3
                            val rotatedChanged = triple._1.map(_ + diff)
                            val intersect      = beacons.intersect(rotatedChanged)
                            (rotatedChanged, intersect, diff)
                        })
                        .find(f => f._2.size >= 12)
                        //We need the rotations of these nodes, and the diff to get to this one?
                        .map(b => (b._1, b._3, sc._2))
                })
                .filter(_.isDefined)
                .map(_.get)
                .toSeq
            val newBeacons                                = beacons ++ relative.flatMap(_._1);
            val gotScanners                               = relative.map(_._3)
            val newScanners = scanners.filter(scanner => !gotScanners.contains(scanner._2))
            val nMap        = scanMap ++ relative.map(sc => sc._3 -> sc._2).toMap
            innerSolve(newScanners, newBeacons, nMap)
        }
    }

    final def apply(location: String): Seq[Scanner] = {
        val inp = Source
            .fromResource(location)
            .getLines
            .foldLeft((Seq.empty[Scanner], Set.empty[Coord3]))((acc, ln) => {
                if (ln.isEmpty || (ln.head == '-' && ln.tail.head == '-')) {
                    (acc._1 :+ new Scanner(acc._2), Set.empty)
                }
                else {
                    val split = ln.split(',')
                    val C     = new Coord3(split(0).toInt, split(1).toInt, split(2).toInt)
                    (acc._1, acc._2 + C)
                }

            })
        (inp._1 :+ new Scanner(inp._2)).filter(_.beacons.nonEmpty)
    }
}
