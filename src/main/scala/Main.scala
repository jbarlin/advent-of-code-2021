import java.util.concurrent.TimeUnit
import lib.DayTemplate

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
        print("\nTotal time: ")
    })
    println("\nAnd ... done!")
}

def runNoPrintDay(day: Int, callable: DayTemplate[?]): Unit = {
    val dayInput = callable.parseInput();
    val partOne  = callable.partOne(dayInput);
    val partTwo  = callable.partTwo(dayInput);
}

def runDay(day: Int, callable: DayTemplate[?]): Unit = {
    print("Day " + day + ":\n\tParse:")
    val dayInput = time(callable.parseInput())
    print("\n\tPart 1:")
    println(time(callable.partOne(dayInput)))
    print("\tPart 2:")
    println(time(callable.partTwo(dayInput)))
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
