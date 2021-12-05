package lib

import scala.compiletime.ops.boolean
import org.apache.commons.lang3.builder.HashCodeBuilder

class Coords(val x: Int, val y: Int) {
    lazy val magnitude          = math.sqrt(math.pow(x, 2) + math.pow(y, 2))
    def +(that: Coords): Coords = {
        new Coords(this.x + that.x, this.y + that.y);
    }

    def -(that: Coords): Coords     = {
        new Coords(this.x - that.x, this.y - that.y);
    }
    override def equals(a: Any)     = a match {
        case c: Coords => c.x == this.x && c.y == this.y
        case _         => false
    }
    override def hashCode(): Int    = {
        new HashCodeBuilder()
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
    override def toString(): String = "Coords(" + x + "," + y + ")"
}
object Orientations extends Enumeration {
    type Orientation = Value
    val Clockwise, Counterclockwise, Colinear = Value
}
object Coords                        {
    def apply(x: Int)(y: Int) = new Coords(x, y)
    def onSegment(pointA: Coords, pointB: Coords): (Coords) => Boolean = (pointToTest: Coords) => {
        pointToTest.x <= Math.max(pointA.x, pointB.x) && pointToTest.x >= Math.min(pointA.x, pointB.x) &&
        pointToTest.y <= Math.max(pointA.y, pointB.y) && pointToTest.y >= Math.min(pointA.y, pointB.y)
    }

    def orientation(pointA: Coords, pointB: Coords): (Coords) => Orientations.Orientation = (pointC: Coords) => {
        val orientation = (pointB.y - pointA.y) * (pointC.x - pointB.x) -
            (pointB.x - pointA.x) * (pointC.y - pointB.y);

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

}
