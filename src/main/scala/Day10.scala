import scala.io.Source
import lib.DayTemplate
import org.apache.commons.lang3.builder.HashCodeBuilder
import scala.annotation.tailrec

final class TokenInformation(val open: Char, val close: Char, val closeScore: Int, val openScore: Int)  {
    override def equals(a: Any)  = a match {
        case c: TokenInformation => c.open == open
        case _                   => false
    }
    override def hashCode(): Int = {
        new HashCodeBuilder(921, 927)
            .append(open)
            .append(close)
            .append(closeScore)
            .toHashCode
    }

    override def toString(): String = {
        "TokenInformation('" + close + "','" + open + "'," + closeScore + ")"
    }
}
final class Token(val tokenInformation: TokenInformation, val isOpen: Boolean) {
    override def toString(): String = {
        "Token('" + tokenInformation.toString + ", " + (if (isOpen) { "open" }
                                                        else { "close" }) + ")"
    }
}
object Token                                                                   {
    val roundBrace  = new TokenInformation('(', ')', 3, 1);
    val squareBrace = new TokenInformation('[', ']', 57, 2);
    val curlyBrace  = new TokenInformation('{', '}', 1197, 3);
    val pointyBrace = new TokenInformation('<', '>', 25137, 4);

    val braces                          = roundBrace :: squareBrace :: curlyBrace :: pointyBrace :: Nil;
    def apply(charToMatch: Char): Token = {
        val matchedToken: TokenInformation = braces.find(br => br.close == charToMatch || br.open == charToMatch).get
        if (matchedToken.close == charToMatch) {
            new Token(matchedToken, false)
        }
        else {
            new Token(matchedToken, true)
        }
    }
}

object Day10 extends DayTemplate[Iterable[Either[Token, List[Token]]]] {
    def parseInput(): Iterable[Either[Token, List[Token]]] = {
        Source
            .fromResource("day10.txt")
            .getLines
            .filter(!_.isBlank)
            .map(
              _.toList.map(Token(_))
            )
            .toList
            .map(tokens => {
                findMissingToken(tokens, Iterable.empty[Token])
            })
    }

    @tailrec
    def findMissingToken(lineLeft: Iterable[Token], opens: Iterable[Token]): Either[Token, List[Token]] = {
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
                val isOpen = currChar.isOpen
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

    def partOne(input: Iterable[Either[Token, List[Token]]]): String = {
        input
            .filter(_.isLeft)
            .map(_.left.toOption.get)
            .map(_.tokenInformation.closeScore)
            .sum
            .toString
    }
    def partTwo(input: Iterable[Either[Token, List[Token]]]): String = {
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
}

//238974358 is too low