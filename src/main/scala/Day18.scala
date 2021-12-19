import lib.DayTemplate
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

sealed trait SnailNumber {
    def addToLeft(toAdd: Int): SnailNumber
    def addToRight(toAdd: Int): SnailNumber
    def magnitude: Long
}

case class NormalNumber(value: Int) extends SnailNumber {
    override def addToLeft(toAdd: Int)  = new NormalNumber(value + toAdd)
    override def addToRight(toAdd: Int) = addToLeft(toAdd)
    override def magnitude              = value
}

case class PairNumbers(left: SnailNumber, right: SnailNumber) extends SnailNumber {
    override def addToLeft(toAdd: Int)  = new PairNumbers(left.addToLeft(toAdd), right)
    override def addToRight(toAdd: Int) = new PairNumbers(left, right.addToRight(toAdd))
    override def magnitude: Long        = 3L * left.magnitude + 2L * right.magnitude
}

object SnailNumber {

    def apply(input: String): (Int, SnailNumber) = {
        val leftParse: (Int, SnailNumber)  = if (input.tail.head == '[') {
            val parseTail = apply(input.tail)
            (parseTail._1, parseTail._2)
        }
        else {
            (1, new NormalNumber(input.tail.head.toInt - 48))
        }
        val left                           = leftParse._2
        val remainingStr                   = input.drop(leftParse._1 + 1)
        val rightParse: (Int, SnailNumber) = if (remainingStr.tail.head == '[') {
            val parseTail = apply(remainingStr.tail)
            (parseTail._1, parseTail._2)
        }
        else {
            (1, new NormalNumber(remainingStr.tail.head.toInt - 48))
        }
        val right                          = rightParse._2
        return (leftParse._1 + rightParse._1 + 3, new PairNumbers(left, right))
    }

    def add(a: SnailNumber, b: SnailNumber): SnailNumber                                                = {
        applyReducers(new PairNumbers(a, b))
    }

    @tailrec
    def applyReducers(number: SnailNumber): SnailNumber = {
        doExplode(number, 0).map(_._3) match {
            case Some(nNumber) => applyReducers(nNumber)
            case None          =>
                split(number) match {
                    case Some(nNumber) => applyReducers(nNumber)
                    case None          => number
                }
        }
    }
    //Left adding number, right adding number, and the final SnailNumber
    //Most of which are maybes :D
    def doExplode(number: SnailNumber, depth: Int = 0): Option[(Option[Int], Option[Int], SnailNumber)] = {
        number match {
            case NormalNumber(_)                                                    => Option.empty
            case PairNumbers(NormalNumber(left), NormalNumber(right)) if depth >= 4 => {
                Option(Option(left), Option(right), NormalNumber(0))
            }
            case PairNumbers(left, right)                                           => {
                val handleLeft  = doExplode(left, depth + 1)
                    .map((leftAdd, rightAdd, newLeft) =>
                        (leftAdd, Option.empty, PairNumbers(newLeft, rightAdd.map(right.addToLeft).getOrElse(right)))
                    )
                val handleRight = doExplode(right, depth + 1)
                    .map((leftAdd, rightAdd, newRight) =>
                        (Option.empty, rightAdd, PairNumbers(leftAdd.map(left.addToRight).getOrElse(left), newRight))
                    )
                handleLeft.orElse(handleRight)
            }
        }
    }

    def split(number: SnailNumber): Option[SnailNumber] = {
        number match {
            case NormalNumber(num) if num >= 10 => {
                val nNum = num.toFloat / 2f
                Option(
                  new PairNumbers(
                    new NormalNumber(nNum.floor.toInt),
                    new NormalNumber(nNum.ceil.toInt)
                  )
                )
            }
            case PairNumbers(left, right)       => {

                val handleLeft  = split(left).map(PairNumbers(_, right))
                val handleRight = split(right).map(PairNumbers(left, _))

                handleLeft.orElse(handleRight)
            }

            case _ => Option.empty
        }
    }
}

type T = Seq[SnailNumber]

object Day18 extends DayTemplate[T] {
    def parseInput(): T = {
        Source
            .fromResource("day18.txt")
            .getLines
            .filter(!_.isBlank)
            .map(
              SnailNumber(_)._2
            )
            .toSeq
    }

    def partOne(input: T): String = {
        input.reduce(SnailNumber.add(_, _)).magnitude.toString
    }

    def partTwo(input: T): String = {
        input
            .combinations(2)
            .toSeq
            .par
            .flatMap[(SnailNumber, SnailNumber)](s => (s.head, s.reverse.head) :: (s.reverse.head, s.head) :: Nil)
            .map(SnailNumber.add(_, _).magnitude)
            .max
            .toString
    }
}
