package lib;

abstract class DayTemplate[T] {
    def parseInput(test: Boolean = false): T;
    def partOne(input: T): String;
    def partTwo(input: T): String;
}
