package lib.day4

final class BingoGameBuilder(val boards: List[BingoBoard] = List.empty, val lines: List[List[BingoTile]] = List.empty) {
    def apply(inp: Array[String]): BingoGameBuilder = {
        if (inp.length == 0) {
            if (lines.length == 0) {
                this
            }
            else {
                new BingoGameBuilder(this.boards ::: new BingoBoard(this.lines) :: Nil, List.empty)
            }
        }
        else {
            val nLine: List[BingoTile] = inp.map(s => new BingoTile(s.toInt)).toList
            new BingoGameBuilder(this.boards, this.lines ::: nLine :: Nil)
        }
    }
}
