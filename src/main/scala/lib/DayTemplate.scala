package lib;

abstract class DayTemplate[T] {
    def parseInput(): T;
    def partOne(input: T): String;
    def partTwo(input: T): String;
}
