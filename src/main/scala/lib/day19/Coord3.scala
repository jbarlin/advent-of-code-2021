package lib.day19

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
}
