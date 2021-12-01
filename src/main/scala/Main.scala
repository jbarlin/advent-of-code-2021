@main def hello: Unit = {
  println("Let's go AoC 2021!")
  //Day 1
  print("Day 1:\n\tPart 1:\t")
  println(time(Day1.partOne()))
  print("\tPart 2:\t")
  println(time(Day1.partTwo()))
  //Day 2
  print("Day 2:")

  println("And ... done!")
}

def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    print("\t(Time:" + (t1 - t0) + "ns)\t")
    result
}

def msg = "I was compiled by Scala 3. :)"
