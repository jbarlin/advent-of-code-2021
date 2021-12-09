import lib.DayTemplate
import scala.io.Source

case class NoteEntry(val signalValues: List[String], val outputValues: List[String]) {
    def apply: Int = {
        val charsInOne: Set[Char] = signalValues.find(_.length == 2).get.toSet
        val charsInSeven: Set[Char] = signalValues.find(_.length == 3).get.toSet
        val charsInFour: Set[Char] = signalValues.find(_.length == 4).get.toSet
        val charsInEight: Set[Char] = signalValues.find(_.length == 7).get.toSet
        val thoseWithFiveSegments: Set[Set[Char]] = signalValues.filter(_.length == 5).map(_.toSet).toSet
        val thoseWithSixSegments: Set[Set[Char]] = signalValues.filter(_.length == 6).map(_.toSet).toSet
        //So we can work out the topLeftAndCenterOptions:
        val topLeftAndCenterOptions: List[Char] = (charsInFour -- charsInOne).toList
        //And only 5 contains the upper left out of the ones that have 5 segments so I guess
        // see if we can make that one fall out?
        val isTopLeftFirst: Boolean = thoseWithFiveSegments.count(_.contains(topLeftAndCenterOptions.head)) == 1
        val topLeft: Char = if (isTopLeftFirst) {
            topLeftAndCenterOptions.head
        } else {
            topLeftAndCenterOptions.tail.head
        }
        val center: Char = if(isTopLeftFirst){
            topLeftAndCenterOptions.tail.head
        }else{
            topLeftAndCenterOptions.head
        }
        val (tCharsInFive: Set[Set[Char]], fiveSegmentsNotDigitFive: Set[Set[Char]]) = thoseWithFiveSegments.partition(_.contains(topLeft));
        val charsInFive: Set[Char] = tCharsInFive.head
        //Hey we know the center, so the one with 6 digits without it can't be 0!
        val (tCharsInZero: Set[Set[Char]], sixSegmentsNotDigitZero: Set[Set[Char]]) = thoseWithSixSegments.partition(!_.contains(center))
        val charsInZero: Set[Char] = tCharsInZero.head
        //So the remaining two that have 5 chars is 2 and 3, and the difference is bottomRight and bottomLeft
        //bottomLeft = 0, 2, 6, 8
        //bottomRight = 0, 1, 3, 4, 5, 6, 7, 8, 9
        //Ok, so 5 only has the bottomRight in common with 1
        val bottomRight: Char = charsInFive.intersect(charsInOne).head
        //Also this means we know 3, since it's the other 5 char one with the bottom right
        //And two would be the other one
        val partitionFive = fiveSegmentsNotDigitFive.partition(_.contains(bottomRight))
        val charsInThree: Set[Char] = partitionFive._1.head
        val charsInTwo: Set[Char] = partitionFive._2.head
        //So we now know 0, 1, 2, 3, 4, 5, 7, and 8 - just 9 and 6 to go?
        //9 and 0 both overlap 1, and we know 0
        //And then that leaves... 6!
        val partitionSix = sixSegmentsNotDigitZero.partition(_.toSet.intersect(charsInOne).size == 2)
        val charsInNine: Set[Char] = partitionSix._1.head
        val charsInSix: Set[Char] = partitionSix._2.head

        val listOfChars = List(
            charsInZero,
            charsInOne,
            charsInTwo,
            charsInThree,
            charsInFour,
            charsInFive,
            charsInSix,
            charsInSeven,
            charsInEight,
            charsInNine
        )

        //And then we use that to work out the digits!
        outputValues.map(output => listOfChars.indexWhere(_ == output.toSet)).mkString.toInt
    }
}

object Day08 extends DayTemplate[Iterable[NoteEntry]] {
    def parseInput(): Iterable[NoteEntry] = {
        Source
            .fromResource("day8.txt")
            .getLines
            .filter(p => p.length > 0)
            .map[Array[String]](p => p.split('|'))
            .map(p => {
                (p.head.trim(), p.tail.head.trim())
            })
            .map(p =>
            new NoteEntry(
              p._1
                  .split(" ")
                  .toList,
              p._2
                  .split(" ")
                  .toList
            )
        ).iterator.to(Iterable)
    }

    val uniqueNumbers: Set[Int] = (2 :: 3 :: 4 :: 7 :: Nil).toSet

    def partOne(input: Iterable[NoteEntry]): String = {
        input
            .map(m => {
                m.outputValues.count(c => uniqueNumbers.contains(c.size))
            })
            .sum
            .toString
    }

    def partTwo(input: Iterable[NoteEntry]): String = {
        input.map(_.apply).sum.toString
    }
}
