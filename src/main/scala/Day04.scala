import lib.DayTemplate
import lib.day4.{BingoBoard, BingoGame, BingoGameBuilder, BingoTile}

import scala.annotation.tailrec
import scala.io.Source

type Plays = Array[Int];
type BingoLine = List[BingoTile];

final object Day04 extends DayTemplate[BingoGame] {
    def parseInput(): BingoGame = {
        val lines = Source
            .fromResource("day4.txt")
            .getLines
            .toList;
        val playsLine = lines.head;
        val remaining = lines.tail.tail;
        val plays = playsLine.split(",").map(s => s.toInt);
        val mp = remaining
            .map(line => line.split(" ").filter(s => s.trim.size != 0))
            .foldLeft(new BingoGameBuilder())(_ (_))
            .boards

        new BingoGame(plays, mp);
    }

    def partOne(input: BingoGame): String = {
        val ans = findWinner(input.plays, input.boards)
        (ans._2 * ans._1.sumUnused).toString
    }

    def partTwo(input: BingoGame): String = {
        val ans = findLastTile(findLoser(input.plays, input.boards))
        (ans._2 * ans._1.sumUnused).toString
    }

    @tailrec
    private def findCondition(plays: Plays, boards: List[BingoBoard], cond: (List[BingoBoard] => Boolean), find: (List[BingoBoard] => BingoBoard)): (Plays, BingoBoard, Int) = {
        val thisPlay = plays.head
        val nextBoards = boards.map(_ (thisPlay))
        if (cond.apply(nextBoards)){
            (plays.tail, find(nextBoards), thisPlay)
        }else{
            findCondition(plays.tail, nextBoards, cond, find)
        }
    }

    private def findWinner(plays: Plays, boards: List[BingoBoard]): (BingoBoard, Int) = {
        val ret = findCondition(plays, boards, (nb) => nb.exists(_.won), (nb) => nb.find(_.won).get)
        (ret._2, ret._3)
    }

    private def findLoser(plays: Plays, boards: List[BingoBoard]): (Plays, BingoBoard) = {
        val ret = findCondition(plays, boards, _.filter(!_.won).size == 1, _.find(!_.won).get)
        (ret._1, ret._2)
    }

    @tailrec
    private def findLastTile(inp: (Plays, BingoBoard)): (BingoBoard, Int) = {
        val thisPlay = inp._1.head
        val newBoard = inp._2(thisPlay)
        if (newBoard.won) {
            (newBoard, thisPlay)
        }
        else {
            findLastTile(inp._1.tail, newBoard)
        }
    }
}
