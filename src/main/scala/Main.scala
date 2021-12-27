import java.util.concurrent.TimeUnit
import lib.{DayTemplate, ExtendingDayTemplate}

@main def hello: Unit = {
    println("Let's go AoC 2021!")
    time({
        runDay(1, Day01);
        runDay(2, Day02);
        runDay(3, Day03);
        runDay(4, Day04);
        runDay(5, Day05);
        runDay(6, Day06);
        runDay(7, Day07);
        runDay(8, Day08);
        runDay(9, Day09);
        runDay(10, Day10);
        runDay(11, Day11);
        runDay(12, Day12);
        runDay(13, Day13);
        runDayExtended(14, Day14);
        // runDay(15, Day15);
        runDay(16, Day16);
        runDay(17, Day17);
        runDay(18, Day18);
        runDay(19, Day19);
        runDay(20, Day20);
        runDay(21, Day21);
        print("\nTotal time: ")
    })
    println("\nAnd ... done!")
}

def runDay(day: Int, callable: DayTemplate[?]): Unit = {
    print("Day " + day + ":\n\tParse:")
    val dayInput = time(callable.parseInput())
    print("\n\tPart 1:")
    println(time(callable.partOne(dayInput)))
    print("\tPart 2:")
    println(time(callable.partTwo(dayInput)))
}

def runDayExtended[T, A](day: Int, callable: ExtendingDayTemplate[T, A]): Unit = {
    print("Day " + day + ":\n\tParse:")
    val dayInput = time(callable.parseInput())
    print("\n\tPart 1:")
    val p1: (String, A) = time(callable.partOne(dayInput))
    println(p1._1)
    print("\tPart 2:")
    println(time(callable.partTwo(dayInput, p1._2)))
}

def time[R](block: => R): R = {
    val t0     = System.nanoTime()
    val result = block // call-by-name
    val t1     = System.nanoTime()
    print(
      "\t(Time:" + TimeUnit.MILLISECONDS
          .convert((t1 - t0), TimeUnit.NANOSECONDS) + "ms)\t"
    )
    result
}
