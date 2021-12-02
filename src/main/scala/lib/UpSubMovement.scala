package lib

case class UpSubMovement(val by: Int) extends lib.SubMovement {
    val myChange = new Coords(0, by);

    def apply(coords: Coords): Coords = {
        coords - myChange
    }
}
