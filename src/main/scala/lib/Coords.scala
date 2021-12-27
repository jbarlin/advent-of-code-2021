package lib

import scala.compiletime.ops.boolean
import org.apache.commons.lang3.builder.HashCodeBuilder

final class Coords(val x: Int, val y: Int) {

    def foldAround(around: (Option[Int], Option[Int])): Coords = {
        val nX = around._1
            .map(p =>
                if (x > p) { 2 * p - x }
                else { x }
            )
            .getOrElse(x)
        val nY = around._2
            .map(p =>
                if (y > p) { 2 * p - y }
                else { y }
            )
            .getOrElse(y)
        Coords(nX)(nY)
    }

    lazy val magnitude          = math.sqrt(math.pow(x, 2) + math.pow(y, 2))
    def +(that: Coords): Coords = {
        new Coords(this.x + that.x, this.y + that.y);
    }

    def to(other: Coords): IndexedSeq[Coords] = {
        val cX = {
            if (this.x > other.x) {
                -1
            } else if (this.y == other.y){
                0
            }
            else {
                1
            }
        }
        val cY = {
            if (this.y > other.y) {
                -1
            } else if (this.y == other.y){
                0
            }
            else {
                1
            }
        }
        (this.x to other.x by cX)
            .flatMap(x => (this.y to other.y by cY).map(y => new Coords(x, y)))
    }

    def addX(x: Int): Coords = {
        this + Coords(x)(0)
    }

    def addY(y: Int): Coords = {
        this + Coords(0)(y)
    }

    def -(that: Coords): Coords     = {
        new Coords(this.x - that.x, this.y - that.y);
    }
    override def equals(a: Any)     = a match {
        case c: Coords => c.x == this.x && c.y == this.y
        case _         => false
    }
    override def hashCode(): Int    = {
        new HashCodeBuilder(923, 929)
            .append(this.x)
            .append(this.y)
            .toHashCode
    }
    def >(that: Coords): Boolean    = {
        this.magnitude > that.magnitude
    }
    def <(that: Coords): Boolean    = {
        this.magnitude < that.magnitude
    }
    def compare(that: Coords)       = this.magnitude.compare(that.magnitude)
    override def toString(): String = "Coords(x:" + x + ",y:" + y + ")"

    def aroundMe(
        min: (Int, Int) = (0, 0),
        max: (Int, Int) = (Int.MaxValue, Int.MaxValue),
        diagonals: Boolean = false
    ): Seq[Coords] = {
        this.addX(-1).addY(-1)
            .to(
                this.addX(1).addY(1)
            )
            .filter(i => i.x >= min._1 && i.x <= max._1 && i.y >= min._2 && i.y <= max._2)
            .filter(p => (diagonals) || (p.x == x || p.y == y))
            .filterNot(p => p.x == x && p.y == y)
            .sortBy(c => (c.y, c.x))
            .toSeq
    }
    lazy val aroundWithDiag = aroundMe((0, 0), (Int.MaxValue, Int.MaxValue), true)
    lazy val aroundNoDiag   = aroundMe((0, 0), (Int.MaxValue, Int.MaxValue), false)
}

object Coords {
    def apply(x: Int)(y: Int)                                          = new Coords(x, y)
    def byY(y: Int)(x: Int)                                            = Coords(x)(y)
    def onSegment(poIntA: Coords, poIntB: Coords): (Coords) => Boolean = (poIntToTest: Coords) => {
        poIntToTest.x <= Math.max(poIntA.x, poIntB.x) && poIntToTest.x >= Math.min(poIntA.x, poIntB.x) &&
        poIntToTest.y <= Math.max(poIntA.y, poIntB.y) && poIntToTest.y >= Math.min(poIntA.y, poIntB.y)
    }

    def orientation(poIntA: Coords, poIntB: Coords): (Coords) => Orientations.Orientation = (poIntC: Coords) => {
        val orientation = (poIntB.y - poIntA.y) * (poIntC.x - poIntB.x) -
            (poIntB.x - poIntA.x) * (poIntC.y - poIntB.y);

        orientation match {
            case 0          => Orientations.Colinear
            case x if x > 0 => Orientations.Clockwise
            case _          => Orientations.Counterclockwise
        }
    }

    def doIntersect(start1: Coords, end1: Coords): (Coords, Coords) => Boolean = (start1: Coords, end2: Coords) => {
        val m1 = orientation(start1, end1)
        val m2 = orientation(start1, end2)
        val o1 = m1(start1);
        val o2 = m1(end2);
        val o3 = m2(start1);
        val o4 = m2(end1);
        (o1 != o2 && o3 != o4) ||
        (o1 == Orientations.Colinear && onSegment(start1, start1)(end1)) ||
        (o2 == Orientations.Colinear && onSegment(start1, end2)(end1)) ||
        (o3 == Orientations.Colinear && onSegment(start1, start1)(end2)) ||
        (o4 == Orientations.Colinear && onSegment(start1, end1)(end2))
    }

    def mapToString(coords: Set[Coords]): String = {
        val maxX = coords.map(c => c.x).max
        val maxY = coords.map(c => c.y).max
        val minX = coords.map(c => c.x).min
        val minY = coords.map(c => c.y).min
        (minY to maxY).foldLeft("")((acc, ycoord) => {
            val coordByY = Coords.byY(ycoord)
            acc +
                (minX to maxX).foldLeft("")((run, xcoord) => {
                    run + (if (coords.contains(coordByY(xcoord))) { "█" }
                           else { " " })
                })
                + "\n"
        })

    }

}
