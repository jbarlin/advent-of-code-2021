package lib.day10

import org.apache.commons.lang3.builder.HashCodeBuilder

final class TokenInformation(val open: Char, val close: Char, val closeScore: Int, val openScore: Int) {
    override def equals(a: Any) = a match {
        case c: TokenInformation => c.open == open
        case _ => false
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
