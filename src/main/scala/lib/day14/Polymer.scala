package lib.day14

type Templates = Map[(Char, Char), Char]

class Polymer(elemPairs: Map[(Char, Char), Long], last: Char) {
    def apply(templates: Templates): Polymer = {
        val newElemPairs: Map[(Char, Char), Long] = elemPairs.toList
            .flatMap(p => {
                val firstE = p._1._1
                val secondE = p._1._2
                val count = p._2
                templates
                    .get(p._1)
                    .map((nChar: Char) => List((firstE, nChar) -> count, (nChar, secondE) -> count))
                    .getOrElse(List((firstE, secondE) -> count))
            })
            .groupMapReduce(_._1)(_._2)(_ + _)
        /*
        That grouped each pair of chars (the first part of the tuple in the list)
            to the current count (the second item in this list)
            and then sum in aggregate to make a map of (Char, Char) -> Long
         */
        new Polymer(newElemPairs, last)
    }

    lazy val elemCounts = elemPairs
        .foldLeft(Map.empty[Char, Long])((acc, p) => {
            val firstE = p._1._1
            val secondE = p._1._2
            val count = p._2
            acc + (firstE -> (acc.getOrElse(firstE, 0L) + count))
        })
        .map(p => p._1 -> (if (p._1 == last) {
            p._2 + 1
        } else {
            p._2
        }))
        .map(_._2)


    def mostCommonCount = elemCounts.max

    def leastCommonCount = elemCounts.min
}

object Polymer {
    def apply(input: String): Polymer = {
        /*
        Group each character in this list by it's identity to 1,
            and then sum them in aggregate to make a map of Char -> Long
         */
        //val elemCounts = input.groupMapReduce(identity)(_ => 1L)(_ + _)
        val elemPairs = input.zip(input.tail).groupMapReduce(identity)(_ => 1L)(_ + _)
        new Polymer(elemPairs, input.last)
    }
}