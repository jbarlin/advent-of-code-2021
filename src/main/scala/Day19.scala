import lib.DayTemplate
import org.apache.commons.lang3.builder.HashCodeBuilder

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.{ParSeq, ParSet}
import scala.io.Source

type T = (Set[Coord3], Map[Int, Coord3])

final case class Coord3(x: Int, y: Int, z: Int) {
    lazy val rotations: Seq[Coord3] = {
        Seq(
          Coord3(x, y, z),
          Coord3(y, z, x),
          Coord3(z, x, y),
          Coord3(-x, z, y),
          Coord3(y, -z, -x),
          Coord3(-z, -x, y),
          Coord3(x, -y, -z),
          Coord3(-y, -z, x),
          Coord3(-y, x, z),
          Coord3(x, -z, y),
          Coord3(-z, y, x),
          Coord3(y, x, -z),
          Coord3(z, y, -x),
          Coord3(y, -x, z),
          Coord3(x, z, -y),
          Coord3(z, -y, x),
          Coord3(-z, x, -y),
          Coord3(-x, -z, -y),
          Coord3(-z, -y, -x),
          Coord3(-y, -x, -z),
          Coord3(-x, -y, z),
          Coord3(-y, z, -x),
          Coord3(z, -x, -y),
          Coord3(-x, y, -z)
        )
    }

    override def toString: String = "Coord3(" + x + "," + y + "," + z + ")"

    def -(that: Coord3): Coord3 = {
        Coord3(x - that.x, y - that.y, z - that.z)
    }

    def +(that: Coord3): Coord3 = {
        Coord3(x + that.x, y + that.y, z + that.z)
    }

    def distance(that: Coord3): Int = {
        (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    }
}

final case class Scanner(beacons: Set[Coord3]) {
    val orientations: Set[Set[Coord3]] = beacons.map(_.rotations).transpose.map(_.toSet).toSet

    override def toString: String = "Scanner(" + beacons + ")"
}

object Scanner {

    def findOtherRelTo(other: (Scanner, Int), beacons: Set[Coord3]): Option[(Set[Coord3], Coord3, Int)] = {
        val myTransform = transform(beacons)

        createRelativeIterator(other._1, beacons)
            .map(myTransform)
            .find(f => f._2.size >= 12)
            //We need the rotations of these nodes, and the diff to get to this one?
            .map(b => (b._1, b._3, other._2))
    }

    def solve(scanners: Seq[Scanner]): (Set[Coord3], Map[Int, Coord3]) = {
        val possibles = scanners.zipWithIndex
        innerSolve(possibles.tail, scanners.head.beacons, Map(0 -> Coord3(0, 0, 0)))
    }

    private def transform(beacons: Set[Coord3]) = (triple: (Set[Coord3], Coord3, Coord3)) => {
        val diff           = triple._2 - triple._3
        val rotatedChanged = triple._1.map(_ + diff)
        val intersect      = beacons.intersect(rotatedChanged)
        (rotatedChanged, intersect, diff)
    }

    private def createRelativeIterator(scanner: Scanner, beacons: Set[Coord3]): ParSet[(Set[Coord3], Coord3, Coord3)] =
        scanner.orientations.par
            .flatMap(rotated => {
                beacons.flatMap(beaconFrom1 => {
                    rotated
                        .map(beaconFrom2 => (rotated, beaconFrom1, beaconFrom2))
                })
            })

    @tailrec
    private def innerSolve(
        scanners: Seq[(Scanner, Int)],
        beacons: Set[Coord3],
        scanMap: Map[Int, Coord3]
    ): (Set[Coord3], Map[Int, Coord3]) = {
        if (scanners.isEmpty) {
            (beacons, scanMap)
        }
        else {
            val (newBeacons, newScanners, newMap) = resolveNewSolverIteration(scanners, beacons, scanMap)

            innerSolve(newScanners, newBeacons, newMap)
        }
    }

    private def resolveNewSolverIteration(
        scanners: Seq[(Scanner, Int)],
        beacons: Set[Coord3],
        scanMap: Map[Int, Coord3]
    ) = {
        val relative: List[(Set[Coord3], Coord3, Int)] = scanners.view
            .map(sc => Scanner.findOtherRelTo(sc, beacons))
            .filter(_.isDefined)
            .map(_.get)
            .toList

        val newBeacons  = beacons ++ relative.flatMap(_._1);
        val gotScanners = relative.map(_._3)
        val newScanners = scanners.filter(scanner => !gotScanners.contains(scanner._2))
        val nMap        = scanMap ++ relative.map(sc => sc._3 -> sc._2).toMap
        (newBeacons, newScanners, nMap)
    }

    def apply(location: String): Seq[Scanner] = {
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

object Day19 extends DayTemplate[T] {
    def parseInput(): T = {
        val fnlInp: Seq[Scanner] = Scanner.apply("day19.txt")
        Scanner.solve(fnlInp)
    }

    def partOne(input: T): String = input._1.size.toString

    def partTwo(input: T): String = input._2
        .map(_._2)
        .toSeq
        .combinations(2)
        .map({ case Seq(a, b) => a.distance(b) })
        .max
        .toString
}
