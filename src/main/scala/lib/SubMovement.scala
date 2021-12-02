package lib;

trait SubMovement {
    def addToCoords(coords: Coords): Coords;
}

case class UpSubMovement(val by: Int) extends lib.SubMovement{
    val myChange = new Coords(0, by);
    def addToCoords(coords: Coords): Coords = {
        coords - myChange
    }
}

case class DownSubMovement(val by: Int) extends lib.SubMovement{
    val myChange = new Coords(0, by);
    def addToCoords(coords: Coords): Coords = {
        coords + myChange
    }
}
case class ForwardSubMovement(val by: Int) extends lib.SubMovement{
    val myChange = new Coords(by, 0);
    def addToCoords(coords: Coords): Coords = {
        coords + myChange
    }
}

