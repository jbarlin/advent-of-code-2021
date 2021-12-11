package lib.day5

import lib.Coords

import scala.annotation.tailrec

final class VentPath(val start: Coords, val end: Coords) {
    final val cX   = {
        if (start.x > end.x) {
            -1
        }
        else {
            1
        }
    }
    final val cY   = {
        if (start.y > end.y) {
            -1
        }
        else {
            1
        }
    }
    val xrange     = (start.x to end.x by cX).toList
    val xmin       = xrange.min
    val xmax       = xrange.max
    val yrange     = (start.y to end.y by cY).toList
    val ymin       = yrange.min
    val ymax       = yrange.max
    val isDiagonal = start.x != end.x && start.y != end.y

    @tailrec
    private def transformMap(state: Map[Coords, Int], currX: Int, currY: Int): Map[Coords, Int] = {
        val nCoord = Coords(currX)(currY)
        val nMap   = state + {
            if (state.contains(nCoord)) {
                (nCoord, state.get(nCoord).get + 1)
            }
            else {
                (nCoord, 1)
            }
        }
        val nextX  = if (start.x != end.x) {
            currX + cX
        }
        else {
            currX
        }
        val nextY  = if (start.y != end.y) {
            currY + cY
        }
        else {
            currY
        }
        if (nextX < xmin || nextX > xmax || nextY < ymin || nextY > ymax) {
            nMap
        }
        else {
            transformMap(nMap, nextX, nextY)
        }
    }

    def apply(states: Map[Coords, Int]): Map[Coords, Int] = {
        transformMap(states, this.start.x, this.start.y)
    }
}

object VentPath {
    def apply(pointA: Coords)(pointB: Coords) = new VentPath(pointA, pointB)
}
