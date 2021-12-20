package lib.day18

import scala.annotation.tailrec

sealed trait SnailNumber {
    def addToLeft(toAdd: Int): SnailNumber
    def addToRight(toAdd: Int): SnailNumber
    def magnitude: Long
}

case class NormalNumber(value: Int) extends SnailNumber {
    override def addToLeft(toAdd: Int): NormalNumber = NormalNumber(value + toAdd)
    override def addToRight(toAdd: Int): NormalNumber = addToLeft(toAdd)
    override def magnitude: Long = value
}


case class PairNumbers(left: SnailNumber, right: SnailNumber) extends SnailNumber {
    override def addToLeft(toAdd: Int): PairNumbers = PairNumbers(left.addToLeft(toAdd), right)
    override def addToRight(toAdd: Int): PairNumbers = PairNumbers(left, right.addToRight(toAdd))
    override def magnitude: Long        = 3L * left.magnitude + 2L * right.magnitude
}


object SnailNumber {

    def apply(input: String): (Int, SnailNumber) = {
        val leftParse: (Int, SnailNumber)  = if (input.tail.head == '[') {
            val parseTail = apply(input.tail)
            (parseTail._1, parseTail._2)
        }
        else {
            (1, NormalNumber(input.tail.head.toInt - 48))
        }
        val left                           = leftParse._2
        val remainingStr                   = input.drop(leftParse._1 + 1)
        val rightParse: (Int, SnailNumber) = if (remainingStr.tail.head == '[') {
            val parseTail = apply(remainingStr.tail)
            (parseTail._1, parseTail._2)
        }
        else {
            (1, NormalNumber(remainingStr.tail.head.toInt - 48))
        }
        val right                          = rightParse._2
        (leftParse._1 + rightParse._1 + 3, PairNumbers(left, right))
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
