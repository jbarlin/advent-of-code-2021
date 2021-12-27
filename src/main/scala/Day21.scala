import lib.DayTemplate
import scala.annotation.tailrec
import scala.collection.mutable

final private case class Player(val square: Int, val score: Long)

type Players = (Player, Player)

object Day21 extends DayTemplate[Players] {
    final def parseInput(): Players = {
        //return (new Player(4, 0), new Player(8, 0))
        return (new Player(6, 0), new Player(1, 0))
    }

    private final val deterministicDice = Iterator.iterate(1)(a => {
        if (a <= 99){
            a + 1
        }else{
            1
        }
    })

    @tailrec
    private final def trackPartOne(player1: Player, player2: Player, rolls: Long = 0): Long = if (
      player2.score >= 1000
    ) {
        rolls * player1.score
    }
    else {
        val nextPos   = (((player1.square + deterministicDice.take(3).sum - 1) % 10) + 1)
        val nextScore = player1.score + nextPos
        trackPartOne(player2, new Player(nextPos, nextScore), rolls + 3)
    }

    final def partOne(input: Players): String = {
        trackPartOne(input._1, input._2, 0).toString
    }

    private val diracMoves = (1 to 3)
        .flatMap(a =>
            (1 to 3)
                .flatMap(b =>
                    (1 to 3)
                        .map(c => a + b + c)
                )
        )
        .foldLeft(Map.empty[Int, Int])
        ((acc, v) => {
            acc + (v -> (acc.getOrElse(v, 0) + 1))
        })

    private val cache = mutable.Map.empty[Players, (Long, Long)]
    private final def trackPartTwo(player1: Player, player2: Player): (Long, Long) = cache.getOrElseUpdate(
      (player1, player2),
      if (player2.score >= 21) {
          (0, 1)
      }
      else {
          diracMoves
            .foldLeft((0L, 0L))
            ((acc, curr) => {
                val (existingWins1, existingWins2) = acc
                val (totalRoll, instances) = curr
                val nextPos = ((player1.square + totalRoll - 1) % 10) + 1
                val nextScore = player1.score + nextPos
                val (wins2, wins1) = trackPartTwo(player2, new Player(nextPos, nextScore))
                (existingWins1 + wins1 * instances, existingWins2 + wins2 * instances)
            })
      }
    )

    final def partTwo(input: Players): String = {
        val wins = trackPartTwo(input._1, input._2)
        wins._1.max(wins._2).toString
    }
}
