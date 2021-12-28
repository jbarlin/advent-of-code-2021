import java.util.concurrent.TimeUnit
import lib.{DayTemplate, ExtendingDayTemplate}
import scala.collection.parallel.CollectionConverters._

private final class DayExec(val test: () => Unit, val prod: (num: Int) => Unit)

private object DayExec {
    final def apply(callable: DayTemplate[?]): DayExec                     = {
        new DayExec(() => runDayNoPrint(callable), (num: Int) => runDay(num, callable))
    }
    final def applyExtended(callable: ExtendingDayTemplate[?, ?]): DayExec = {
        new DayExec(() => runDayExtNoPrint(callable), (num: Int) => runDayExtended(num, callable))
    }
}

private val dayMap: Map[Int, DayExec] = Map(
  (1, DayExec(Day01)),
  (2, DayExec(Day02)),
  (3, DayExec(Day03)),
  (4, DayExec(Day04)),
  (5, DayExec(Day05)),
  (6, DayExec(Day06)),
  (7, DayExec(Day07)),
  (8, DayExec(Day08)),
  (9, DayExec(Day09)),
  (10, DayExec(Day10)),
  (11, DayExec(Day11)),
  (12, DayExec(Day12)),
  (13, DayExec(Day13)),
  (14, DayExec.applyExtended(Day14)),
  (16, DayExec(Day16)),
  (17, DayExec(Day17)),
  (18, DayExec(Day18)),
  //(19, DayExec(Day19)),
  //(20, DayExec(Day20)),
  (21, DayExec(Day21))
);

@main def hello: Unit = {
    println("Let's go AoC 2021!")
    if (false) {
        println("Warming up the JVM")
        time({
            for (x <- 1 to 1000) {
                dayMap.par
                    .foreach((num, exec) => {
                        if (num == 19 || num == 20) {
                            if (x % 10 == 3 || x > 900) {
                                exec.test.apply
                            }
                        }
                        else {
                            exec.test.apply
                        }
                    })
                if ((x < 900 && (x % 100 == 2 || x % 100 == 3)) || (x >= 900 && x % 50 == 0)) {
                    println("\tExec run " + x)
                }
            }
            print("Warming the VM took: ")
        })
        println("\nRunning production!")
    }

    time({
        dayMap.foreach((num, exec) => {
            exec.prod.apply(num)
        })
        print("\nTotal time: ")
    })
    println("\nAnd ... done!")
}

def runDayNoPrint(callable: DayTemplate[?], test: Boolean = true): Unit = {
    val dayInput = callable.parseInput(test)
    callable.partOne(dayInput)
    callable.partTwo(dayInput)
}

def runDay(day: Int, callable: DayTemplate[?], test: Boolean = false): Unit = {
    print("Day " + day + ":\n\tParse:")
    val dayInput = time(callable.parseInput(test))
    print("\n\tPart 1:")
    println(time(callable.partOne(dayInput)))
    print("\tPart 2:")
    println(time(callable.partTwo(dayInput)))
}

def runDayExtNoPrint[T, A](callable: ExtendingDayTemplate[T, A], test: Boolean = true): Unit = {
    val dayInput        = callable.parseInput(test)
    val p1: (String, A) = callable.partOne(dayInput)
    callable.partTwo(dayInput, p1._2)
}

def runDayExtended[T, A](day: Int, callable: ExtendingDayTemplate[T, A], test: Boolean = false): Unit = {
    print("Day " + day + ":\n\tParse:")
    val dayInput        = time(callable.parseInput(test))
    print("\n\tPart 1:")
    val p1: (String, A) = time(callable.partOne(dayInput))
    println(p1._1)
    print("\tPart 2:")
    println(time(callable.partTwo(dayInput, p1._2)))
}

def time[R](block: => R): R = {
    val t0       = System.nanoTime()
    val result   = block // call-by-name
    val t1       = System.nanoTime()
    val convertM = TimeUnit.MILLISECONDS
        .convert((t1 - t0), TimeUnit.NANOSECONDS)
    if (convertM < 20000) {
        print("\t(Time:" + convertM + "ms)\t")
    }
    else {
        val convertS = TimeUnit.SECONDS
            .convert((t1 - t0), TimeUnit.NANOSECONDS)
        print("\t(Time:" + convertS + "s)\t")
    }

    result
}
