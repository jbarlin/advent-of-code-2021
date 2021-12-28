import lib.DayTemplate
import lib.day10.{Token, TokenInformation}
import org.apache.commons.lang3.builder.HashCodeBuilder
import scala.collection.parallel.CollectionConverters._

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.ParSeq

object Day10 extends DayTemplate[ParSeq[Either[Token, List[Token]]]] {
    def parseInput(test: Boolean): ParSeq[Either[Token, List[Token]]] = {
        Source
            .fromResource(
              if (!test) { "day10.txt" }
              else { "day10-test.txt" }
            )
            .getLines
            .filter(!_.isBlank)
            .toList
            .par
            .map(
              _.toList.map(Token(_))
            )
            .map(tokens => {
                findMissingToken(tokens, Iterable.empty[Token])
            })
    }

    def partOne(input: ParSeq[Either[Token, List[Token]]]): String = {
        input
            .filter(_.isLeft)
            .map(_.left.toOption.get)
            .map(_.tokenInformation.closeScore)
            .sum
            .toString
    }

    def partTwo(input: ParSeq[Either[Token, List[Token]]]): String = {
        val remaining = input
            .filter(_.isRight)
            .map(_.toOption.get)
            .map(_.map(_.tokenInformation.openScore.toLong))
            .map(l => {
                val b = l.foldLeft(0L)((acc, b) => acc * 5 + b)
                b
            })
            .toList
            .sorted
        remaining(remaining.size / 2).toString
    }

    @tailrec
    private def findMissingToken(lineLeft: Iterable[Token], opens: Iterable[Token]): Either[Token, List[Token]] = {
        if (lineLeft.isEmpty) {
            Right(opens.toList)
        }
        else {
            val currChar: Token = lineLeft.head;
            if ((!currChar.isOpen) && opens.isEmpty) {
                Right(opens.toList)
            }
            else {
                val tail    = lineLeft.tail
                val cOpen   = opens.headOption
                val nxOpens = if (currChar.isOpen) {
                    List(currChar) ++ opens
                }
                else {
                    opens.tail
                }
                val isOpen  = currChar.isOpen
                if ((!isOpen) && (!opens.head.tokenInformation.equals(currChar.tokenInformation))) {
                    Left(currChar);
                }
                else {
                    findMissingToken(
                      tail,
                      nxOpens
                    )
                }
            }
        }
    }
}
