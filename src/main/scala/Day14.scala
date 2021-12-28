import lib.ExtendingDayTemplate
import lib.day14.Polymer

import scala.io.Source
import javax.xml.transform.Templates
import scala.annotation.tailrec

type Templates = Map[(Char, Char), Char]

type Day14Type = (Polymer, Templates)

object Day14 extends ExtendingDayTemplate[Day14Type, Polymer] {
    def parseInput(test: Boolean): Day14Type = {
        val inp       = Source
            .fromResource(
              if (!test) { "day14.txt" }
              else { "day14-test.txt" }
            )
            .getLines
            .filter(!_.isBlank)
            .toList;
        val polyStr   = inp.head
        val templates = inp.tail
            .map(str => {
                val in = str.toList
                (in.head, in.tail.head) -> in.last
            })
            .toMap
        (Polymer(polyStr), templates)
    }

    @tailrec
    def transformInputNTimes(input: Polymer, templates: Templates, count: Int): Polymer = {
        if (count == 0) {
            input
        }
        else {
            transformInputNTimes(input(templates), templates, count - 1)
        }
    }

    def partOne(input: Day14Type): (String, Polymer) = {
        val transformed = transformInputNTimes(input._1, input._2, 10);
        ((transformed.mostCommonCount - transformed.leastCommonCount).toString, transformed)
    }

    def partTwo(input: Day14Type, fromOne: Polymer): String = {
        val transformed = transformInputNTimes(fromOne, input._2, 40 - 10)
        (transformed.mostCommonCount - transformed.leastCommonCount).toString
    }
}
