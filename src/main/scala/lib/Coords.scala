package lib

class Coords(val x: Int, val y: Int) {
    def +(that: Coords): Coords = {
        new Coords(this.x + that.x, this.y + that.y);
    }

    def -(that: Coords): Coords = {
        new Coords(this.x - that.x, this.y - that.y);
    }
}
