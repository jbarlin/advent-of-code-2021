package lib

abstract class ExtendingDayTemplate[T, A] {
    def parseInput(test: Boolean = false): T
    def partOne(input: T): (String, A);
    def partTwo(input: T, fromOne: A): String;
}
