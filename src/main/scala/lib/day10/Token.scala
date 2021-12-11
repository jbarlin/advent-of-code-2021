package lib.day10

final class Token(val tokenInformation: TokenInformation, val isOpen: Boolean) {
    override def toString(): String = {
        "Token('" + tokenInformation.toString + ", " + (if (isOpen) {
            "open"
        }
        else {
            "close"
        }) + ")"
    }
}

object Token {
    val roundBrace = new TokenInformation('(', ')', 3, 1);
    val squareBrace = new TokenInformation('[', ']', 57, 2);
    val curlyBrace = new TokenInformation('{', '}', 1197, 3);
    val pointyBrace = new TokenInformation('<', '>', 25137, 4);

    val braces = roundBrace :: squareBrace :: curlyBrace :: pointyBrace :: Nil;

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