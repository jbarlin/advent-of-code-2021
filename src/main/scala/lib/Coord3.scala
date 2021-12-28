package lib

final case class Coord3(x: Int, y: Int, z: Int) {
    final lazy val rotations: Seq[Coord3] = {
        Seq(
          Coord3(x, y, z),
          Coord3(y, z, x),
          Coord3(z, x, y),
          Coord3(-x, z, y),
          Coord3(y, -z, -x),
          Coord3(-z, -x, y),
          Coord3(x, -y, -z),
          Coord3(-y, -z, x),
          Coord3(-y, x, z),
          Coord3(x, -z, y),
          Coord3(-z, y, x),
          Coord3(y, x, -z),
          Coord3(z, y, -x),
          Coord3(y, -x, z),
          Coord3(x, z, -y),
          Coord3(z, -y, x),
          Coord3(-z, x, -y),
          Coord3(-x, -z, -y),
          Coord3(-z, -y, -x),
          Coord3(-y, -x, -z),
          Coord3(-x, -y, z),
          Coord3(-y, z, -x),
          Coord3(z, -x, -y),
          Coord3(-x, y, -z)
        )
    }

    override def toString: String = "Coord3(" + x + "," + y + "," + z + ")"

    final def -(that: Coord3): Coord3 = {
        Coord3(x - that.x, y - that.y, z - that.z)
    }

    final def +(that: Coord3): Coord3 = {
        Coord3(x + that.x, y + that.y, z + that.z)
    }

    final def distance(that: Coord3): Int = {
        (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    }

    def to(other: Coord3): LazyList[Coord3] = {
        val cX = {
            if (this.x > other.x) {
                -1
            }
            else if (this.y < other.y) {
                1
            }
            else {
                1
            }
        }
        val cY = {
            if (this.y > other.y) {
                -1
            }
            else if (this.y < other.y) {
                1
            }
            else {
                1
            }
        }
        val cZ = {
            if (this.z > other.z) {
                -1
            }
            else if (this.z < other.z) {
                1
            }
            else {
                1
            }
        }
        (this.x to other.x by cX)
            .to(LazyList)
            .flatMap(x => {
                (this.y to other.y by cY)
                    .flatMap(y => {
                        (this.z to other.z by cZ).map(z => new Coord3(x, y, z))
                    })
            })
    }

}
