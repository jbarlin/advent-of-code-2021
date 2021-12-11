package lib
import scala.annotation.tailrec

object RecursiveUtils {

    @tailrec
    def applyStackOperationsToMap[A, B, C](
        inMap: Map[A, B],
        toDo: Iterable[C],
        operation: (Map[A, B], Iterable[C], Set[C]) => (Map[A, B], Iterable[C], Set[C]),
        done: Set[C]
    ): (Map[A, B], Set[C]) = {
        if (toDo.isEmpty) {
            (inMap, done)
        }
        else {
            val completed = operation(inMap, toDo, done);
            applyStackOperationsToMap(completed._1, completed._2, operation, completed._3)
        }
    }

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

    def countUntilConditionWithPrep[T](
        start: T,
        operation: T => T,
        condition: (T, Int) => Boolean,
        prepForNext: T => T = (a: T) => a
    ): Int = {
        countUntilConditionWithPrepRec(start, (a: T, _) => operation(a), condition, 0, prepForNext)._2
    }

    @tailrec
    private def countUntilConditionWithPrepRec[T](
        currAt: T,
        op: (T, Int) => T,
        condition: (T, Int) => Boolean,
        count: Int,
        prepForNext: T => T = (a: T) => a
    ): (T, Int) = {
        val nCount = count + 1
        val next   = op.apply(currAt, nCount);
        if (condition(next, nCount)) {
            (next, nCount)
        }
        else {
            countUntilConditionWithPrepRec(prepForNext(next), op, condition, nCount, prepForNext)
        }
    }
}
