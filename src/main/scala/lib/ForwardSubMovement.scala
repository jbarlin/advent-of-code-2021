package lib

case class ForwardSubMovement(val by: Int) extends lib.SubMovement {
    val myChange = new Coords(by, 0);

    def apply(coords: Coords): Coords = {
        coords + myChange
    }
}
