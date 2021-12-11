package lib.day5

import lib.Coords

import scala.annotation.tailrec

final class VentPath(val start: Coords, val end: Coords) {
    final val cX = {
        if (start.x > end.x) {
            -1
        }
        else {
            1
        }
    }
    final val cY = {
        if (start.y > end.y) {
            -1
        }
        else {
            1
        }
    }
    val xrange = start.x to end.x by cX
    val yrange = start.y to end.y by cY
    val isDiagonal = start.x != end.x && start.y != end.y

    def apply(states: Map[Coords, Int]): Map[Coords, Int] = {
        @tailrec
        def innerLoop(state: Map[Coords, Int], currX: Int, currY: Int): Map[Coords, Int] = {
            val nCoord = Coords(currX)(currY)
            val nMap = state + {
                if (state.contains(nCoord)) {
                    (nCoord, state.get(nCoord).get + 1)
                }
                else {
                    (nCoord, 1)
                }
            }
            val nextX = if (start.x != end.x) {
                currX + cX
            }
            else {
                currX
            }
            val nextY = if (start.y != end.y) {
                currY + cY
            }
            else {
                currY
            }
            if ((!this.xrange.contains(nextX)) || (!this.yrange.contains(nextY))) {
                nMap
            }
            else {
                innerLoop(nMap, nextX, nextY)
            }
        }

        innerLoop(states, this.start.x, this.start.y)
    }
}

final object VentPath {
    def apply(pointA: Coords): (Coords) => VentPath = { (pointB: Coords) => {
        if (pointA.x == pointB.x) {
            if (pointA.y < pointB.y) {
                new VentPath(pointA, pointB)
            }
            else {
                new VentPath(pointB, pointA)
            }
        }
        else {
            if (pointA.x < pointB.x) {
                new VentPath(pointA, pointB)
            }
            else {
                new VentPath(pointB, pointA)
            }
        }
    }
    }
}