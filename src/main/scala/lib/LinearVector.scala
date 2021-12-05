package lib

class LinearVector(val m: Int, val b: Int) {
    def atX(x: Int): Int = {
        m * x + b
    }
    def atY(y: Int): Int = {
        (y - b) / m
    }

    def intersection(that: LinearVector): Option[Coords] = {
        if (that.m == this.m) {
            Option.empty
        }
        else {
            val x = (that.b - this.b) / (that.m - this.m)
            val y = this.atX(x);
            return Option(new Coords(x, y));
        }
    }
}

object LinearVector {
    def apply(pointA: Coords): (Coords => LinearVector) = (pointB: Coords) => {
        val dx = pointA.x - pointB.x
        val dy = pointA.y - pointB.y
        val m  = if (dx == 0) {
            0
        }
        else {
            (pointA.y - pointB.y) / (pointA.x - pointB.x)
        }
        val b  = pointA.y - m * pointA.x
        new LinearVector(m, b)
    }
}
