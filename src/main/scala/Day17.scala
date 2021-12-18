import lib.DayTemplate

final case class Target(val xRange: Range, val yRange: Range){
    val xmin = xRange.min
    val xmax = xRange.max
    val ymin = yRange.min
    val ymax = yRange.max
}

object Day17 extends DayTemplate[Target] {
  
    def parseInput() = {
        val testA = new Target(20 to 30, -10 to -5)
        val myProblem = new Target(244 to 303, -91 to -54)
        return myProblem
    }

    private def triangular(n: Int): Int = {
        (n.abs) * (n.abs - 1) / 2
    }

    private def inverseTri(n: Int): Int = {
        scala.math.pow(n * 2, 0.5).toInt
    }

    def partOne(input: Target): String = {
        if (triangular(inverseTri(input.xmin)) != input.xmin && (triangular(inverseTri(input.xmin) + 1)) > input.xmax) {
            throw new NotImplementedError("Only wrote for some triangular number in x")
        }else{
            triangular(input.ymin).toString
        }
    }

    def partTwo(input: Target): String = {
        (inverseTri(input.xmin) to input.xmax + 1)
            .foldLeft(0)((countX, velX) => {
                (input.ymin to input.ymin.abs)
                    .foldLeft(countX)((count, velY) => {
                        count + checkIfVelGood(velX, velY, input)
                    })
            })
            .toString
    }

    private def checkIfVelGood(vx: Int, vy: Int, input: Target): Int = {
        Iterator.iterate((0, 0, vx, vy, Option.empty[Int]))
            ((oX, oY, oVX, oVY, ind) => {
                val x = oX + oVX
                val y = oY + oVY
                val vx = oVX + (if (oVX == 0) {0} else if (oVX > 0) {-1} else {1})
                val vy = oVY - 1
                val pair = (input.xmin <= x && x <= input.xmax, input.ymin <= y && y <= input.ymax)
                pair match {
                    case (true, true) => (x, y, vx, vy,Option(1))
                    case (false, _) if vx == 0 => (x, y, vx, vy, Option(0))
                    case (_, false) if vy < 0 && y < input.ymin => (x, y, vx, vy, Option(0))
                    case _ => (x, y, vx, vy, Option.empty)
                }
            })
            .dropWhile(_._5.isEmpty)
            .next()
            ._5
            .getOrElse(0)
    }

}
