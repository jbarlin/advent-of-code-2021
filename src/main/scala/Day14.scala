import lib.ExtendingDayTemplate
import scala.io.Source
import javax.xml.transform.Templates
import scala.annotation.tailrec

type Templates = Map[(Char, Char), Char]

class Polymer(elemCounts: Map[Char, Long], elemPairs: Map[(Char, Char), Long]) {
    def apply(templates: Templates): Polymer = {
        val newElemPairs: Map[(Char, Char), Long] = elemPairs.toList.flatMap(p => {
            val firstE = p._1._1
            val secondE = p._1._2
            val count = p._2
            templates.get(p._1)
                .map((nChar: Char) => List((firstE, nChar) -> count, (nChar, secondE) -> count))
                .getOrElse(List((firstE, secondE) -> count))
        })
        .groupMapReduce(_._1)(_._2)(_ + _)
        /*
        That grouped each pair of chars (the first part of the tuple in the list)
            to the current count (the second item in this list)
            and then sum in aggregate to make a map of (Char, Char) -> Long
        */
        val newElemCounts: Map[Char, Long] = elemPairs.foldLeft(elemCounts)((acc, p) => {
            val firstE = p._1._1
            val secondE = p._1._2
            val count = p._2
            templates.get(p._1)
                .map(nChar => acc + (nChar -> (acc.getOrElse(nChar, 0L) + count)))
                .getOrElse(acc)
        })
        new Polymer(newElemCounts, newElemPairs)
    }

    def mostCommonCount  = elemCounts.map(_._2).max
    def leastCommonCount = elemCounts.map(_._2).min
}

object Polymer {
    def apply(input: String): Polymer = {
        /*
        Group each character in this list by it's identity to 1,
            and then sum them in aggregate to make a map of Char -> Long
        */
        val elemCounts = input.groupMapReduce(identity)(_ => 1L)(_ + _)
        val elemPairs  = input.zip(input.tail).groupMapReduce(identity)(_ => 1L)(_ + _)
        new Polymer(elemCounts, elemPairs)
    }
}

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
