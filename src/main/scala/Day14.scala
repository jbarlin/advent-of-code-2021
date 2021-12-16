import lib.ExtendingDayTemplate
import lib.day14.Polymer

import scala.io.Source
import javax.xml.transform.Templates
import scala.annotation.tailrec

type Templates = Map[(Char, Char), Char]

type T = (Polymer, Templates)

object Day14 extends ExtendingDayTemplate[T, Polymer] {
    def parseInput(): T = {
        val inp       = Source
            .fromResource("day14.txt")
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

    def partOne(input: T): (String, Polymer) = {
        val transformed = transformInputNTimes(input._1, input._2, 10);
        ((transformed.mostCommonCount - transformed.leastCommonCount).toString, transformed)
    }

    def partTwo(input: T, fromOne: Polymer): String = {
        val transformed = transformInputNTimes(fromOne, input._2, 40 - 10)
        (transformed.mostCommonCount - transformed.leastCommonCount).toString
    }
}
