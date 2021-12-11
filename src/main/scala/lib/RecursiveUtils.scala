package lib
import scala.annotation.tailrec

final object RecursiveUtils {
    @tailrec
    def repeatUntilNoChanges[T](currAt: T, op: T => T): T = {
        val next = op.apply(currAt);
        if (next == currAt) {
            next
        }
        else {
            repeatUntilNoChanges(next, op)
        }
    }

    def countUntilConditionWithPrep[T](start: T, op: T => T, condition: T => Boolean, prepForNext: T => T = (a: T) => a): Int = {
        countUntilConditionWithPrepRec(start, op, condition, 0, prepForNext)
    }

    @tailrec
    private def countUntilConditionWithPrepRec[T](currAt: T, op: T => T, condition: T => Boolean, count: Int, prepForNext: T => T = (a: T) => a): Int = {
        val next = op.apply(currAt);
        if (condition(next)){
            count + 1
        }else{
            countUntilConditionWithPrepRec(prepForNext(next), op, condition, count + 1, prepForNext)
        }
    }
}
