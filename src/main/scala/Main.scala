import java.util.concurrent.TimeUnit

@main def hello: Unit = {
  println("Let's go AoC 2021!")
  //Day 1
  print("Day 1:\n\tParse:")
  val dayOneInput: List[Int] = time(Day1.parseInput())
  print("\n\tPart 1:")
  println(time(Day1.partOne(dayOneInput)))
  print("\tPart 2:")
  println(time(Day1.partTwo(dayOneInput)))
  //Day 2
  print("Day 2:\n\tParse:")
  val dayTwoInput = time(Day2.parseInput())
  print("\n\tPart 1:")
  println(time(Day2.partOne(dayTwoInput)))
  print("\tPart 2:")
  println(time(Day2.partTwo(dayTwoInput)))

  println("And ... done!")
}

def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    print("\t(Time:" + TimeUnit.MILLISECONDS.convert((t1 - t0), TimeUnit.NANOSECONDS) + "ms)\t")
    result
}
