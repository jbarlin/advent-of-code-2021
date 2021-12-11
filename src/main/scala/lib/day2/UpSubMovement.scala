package lib.day2

import lib.Coords

case class UpSubMovement(val by: Int) extends SubMovement {
    val myChange = new Coords(0, by);

    def apply(coords: Coords): Coords = {
        coords - myChange
    }
}
