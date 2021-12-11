package lib.day2

import lib.Coords

trait SubMovement {
    def apply(coords: Coords): Coords;
}
