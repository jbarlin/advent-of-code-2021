package lib.day2

import lib.Coords

case class ForwardSubMovement(val by: Int) extends SubMovement {
    val myChange = new Coords(by, 0);

    def apply(coords: Coords): Coords = {
        coords + myChange
    }
}
