import lib.DayTemplate
import scala.io.Source
import scala.annotation.tailrec

class BingoTile(val value: Int, val used: Boolean = false) {
    def apply(valueLookup: Int): BingoTile = {
        return BingoTile(this.value, this.used || this.value == valueLookup)
    }
}

type Plays = Array[Int];
type BingoLine = List[BingoTile];

class BingoBoard(val lines: List[BingoLine], val won: Boolean = false)                                     {
    def apply(play: Int): BingoBoard = {
        val lines: List[BingoLine] = this.lines
            .map(_.map(_(play)))
        new BingoBoard(lines, this.won || lines.exists(_.forall(_.used)) || lines.transpose.exists(_.forall(_.used)))
    }

    lazy val sumUnused: Int = {
        lines.flatten.filter(!_.used).map(_.value).sum
    }
}

class BingoGameBuilder(val boards: List[BingoBoard] = List.empty, val lines: List[BingoLine] = List.empty) {
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
            val nLine: BingoLine = inp.map(s => new BingoTile(s.toInt)).toList
            new BingoGameBuilder(this.boards, this.lines ::: nLine :: Nil)
        }
    }
}

class BingoGame(val plays: Plays, val boards: List[BingoBoard])                                            {}

object Day04 extends DayTemplate[BingoGame] {
    def parseInput(): BingoGame = {
        val lines     = Source
            .fromResource("day4.txt")
            .getLines
            .toList;
        val playsLine = lines.head;
        val remaining = lines.tail.tail;
        val plays     = playsLine.split(",").map(s => s.toInt);
        val mp        = remaining
            .map(line => line.split(" ").filter(s => s.trim.size != 0))
            .foldLeft(new BingoGameBuilder())(_(_))
            .boards

        new BingoGame(plays, mp);
    }

    @tailrec
    def findWinner(plays: Plays, boards: List[BingoBoard]): (BingoBoard, Int) = {
        val thisPlay   = plays.head
        val nextBoards = boards.map(_(thisPlay))
        if (nextBoards.exists(_.won)) {
            (nextBoards.find(_.won).get, thisPlay)
        }
        else {
            findWinner(plays.tail, nextBoards)
        }
    }

    def partOne(input: BingoGame): String = {
        val ans = findWinner(input.plays, input.boards)
        (ans._2 * ans._1.sumUnused).toString
    }

    @tailrec
    def findLoser(plays: Plays, boards: List[BingoBoard]): (Plays, BingoBoard) = {
        val thisPlay   = plays.head
        val nextBoards = boards.map(_(thisPlay)).filter(!_.won)
        if (nextBoards.length == 1) {
            (plays.tail, nextBoards.head)
        }
        else {
            findLoser(plays.tail, nextBoards)
        }
    }

    @tailrec
    def findLastTile(inp: (Plays, BingoBoard)): (BingoBoard, Int) = {
        val thisPlay = inp._1.head
        val newBoard = inp._2(thisPlay)
        if (newBoard.won) {
            (newBoard, thisPlay)
        }
        else {
            findLastTile(inp._1.tail, newBoard)
        }
    }
    def partTwo(input: BingoGame): String = {
        val ans = findLastTile(findLoser(input.plays, input.boards))
        (ans._2 * ans._1.sumUnused).toString
    }
}
