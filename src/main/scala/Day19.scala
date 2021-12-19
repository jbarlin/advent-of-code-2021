import org.apache.commons.lang3.builder.HashCodeBuilder
import scala.collection.parallel.CollectionConverters._
import scala.collection.SeqView

final class Coord3(val x: Int, val y: Int, val z: Int) {

    def -(that: Coord3): Coord3 = {
        Coord3(x - that.x, y - that.y, z - that.z)
    }
    def +(that: Coord3): Coord3 = {
        Coord3(x + that.x, y + that.y, z + that.z)
    }

    def distance(that: Coord3) = {
        (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    }

    override def hashCode(): Int = (new HashCodeBuilder(6703, 5903)).append(x).append(y).append(z).toHashCode

    override def equals(o: Any) = o match {
        case that: Coord3 => that.x == this.x && that.y == this.y && that.z == this.z
        case _            => false
    }

    private def rotateZ(deg: Int): Coord3 = {
        val cos = Math.cos(Math.toRadians(deg))
        val sin = Math.sin(Math.toRadians(deg))
        new Coord3((cos * x - sin * y).toInt, (sin * x + cos * y).toInt, z)
    }

    private def rotateX(deg: Int): Coord3 = {
        val cos = Math.cos(Math.toRadians(deg))
        val sin = Math.sin(Math.toRadians(deg))
        new Coord3(x, (cos * y - sin * z).toInt, (sin * y + cos * z).toInt)
    }

    private def rotateY(deg: Int): Coord3 = {
        val cos = Math.cos(Math.toRadians(deg))
        val sin = Math.sin(Math.toRadians(deg))
        new Coord3((cos * x - sin * z).toInt, y, (sin * x + cos * z).toInt)
    }

    lazy val rotations: Seq[Coord3] = {
        (0 :: 90 :: 180 :: 270 :: Nil).permutations
            .map(quad => this.rotateX(quad(0)).rotateY(quad(1)).rotateZ(quad(2)))
            .toSet
            .toSeq
    }
}

final class Scanner(val beacons: Set[Coord3]) {

    override def equals(o: Any): Boolean = o match {
       case that: Scanner => that.beacons == beacons
       case _ => false
    }

    lazy val orientations = beacons.toSeq.map(_.rotations).transpose.map(_.toSet)

    def findOtherRelToMe(other: Scanner): Option[(Set[Coord3], Coord3, Scanner)] = {
        other.orientations.par
            .flatMap(rotated => {
                beacons.view.flatMap(beaconFrom1 => {
                    other.beacons
                        .view
                        .map(beaconFrom2 => (rotated, beaconFrom1, beaconFrom2))
                })
            })
        .map((rotated, beaconFrom1, beaconFrom2) => {
            val diff      = beaconFrom1 - beaconFrom2
            val intersect = rotated.map(_ + diff) & beacons
            (rotated, intersect, diff)
        })
        .find(f => f._2.size >= 12)
        //We need the rotations of these nodes, and the diff to get to this one?
        .map(b => (b._1, b._3, other))
    }
}

object Scanner{
    private def innerSolve(scanners: Seq[Scanner], beacons: Set[Coord3]): Set[Coord3] = {
        if (scanners.isEmpty){
            beacons
        }else{
            val baseScanner = new Scanner(beacons)
            val relative: (Set[Coord3], Coord3, Scanner) = scanners.view.map(sc => baseScanner.findOtherRelToMe(sc)).find(_.isDefined).get()
            innerSolve(scanners.filter(_ != relative._3), beacons :+ relative._1.map(_ + relative._2))
        }
    }

    def solve(scanners: Seq[Scanner]): Set[Coord3] = {
        val possibles = scanners
        innerSolve(possibles.tail, possibles.head.beacons)
    }
}

object Day19 extends DayTemplate[Seq[Scanner]] {
    def parseInput(): Seq[Scanner] = {
         val inp = Source.readResource("day19-test-a.txt").getLines.foldLeft((Seq[Scanner], Set[Coords3]))((acc, ln) => {
             if (ln.isEmpty || ln.head == '-' ) {
                 (acc._1 :+ new Scanner(acc._2), Set.empty)
             } else {
                 val split = ln.split(',')
                 val C = new Coord3(ln(0).toInt, ln(1).toInt, ln(2).toInt)
                 (acc._1, acc._2 :+ C)
             }

         })
         (inp._1 :+ new Scanner(inp._2)).filter(!_.beacons.isEmpty)
    }

    def partOne(input: Seq[Scanner]): String = Scanner.solve(input).size.toString

    def partTwo(input: Seq[Scanner]): String = ""
}
