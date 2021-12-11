package lib.day4

final class BingoBoard(val lines: List[List[BingoTile]], val won: Boolean = false) {
    def apply(play: Int): BingoBoard = {
        val lines: List[List[BingoTile]] = this.lines
          .map(_.map(_ (play)))
        new BingoBoard(lines, this.won || lines.exists(_.forall(_.used)) || lines.transpose.exists(_.forall(_.used)))
    }

    lazy val sumUnused: Int = {
        lines.flatten.filter(!_.used).map(_.value).sum
    }
}
