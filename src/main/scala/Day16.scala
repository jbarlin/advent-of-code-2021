import lib.DayTemplate
import scala.io.Source
import scala.util.Try

abstract trait HexPacket

final case class Literal(val v: Long, val i: Long, val value: Long)                  extends HexPacket
final case class Operator(val v: Long, val i: Long, val subPackets: List[HexPacket]) extends HexPacket

type T = Seq[Int]

object Day16 extends DayTemplate[HexPacket] {

    private def parseBin(in: Seq[Int]): Long = {
        java.lang.Long.parseLong(in.foldLeft("")((a, b) => a + b.toString), 2)
    }

    private val hToB = Map(
      '0' -> "0000",
      '1' -> "0001",
      '2' -> "0010",
      '3' -> "0011",
      '4' -> "0100",
      '5' -> "0101",
      '6' -> "0110",
      '7' -> "0111",
      '8' -> "1000",
      '9' -> "1001",
      'A' -> "1010",
      'B' -> "1011",
      'C' -> "1100",
      'D' -> "1101",
      'E' -> "1110",
      'F' -> "1111"
    )

    private def translate(input: T): (Int, HexPacket) = {
        val version = parseBin(input.slice(0, 3))
        val typeId  = parseBin(input.slice(3, 6))
        if (typeId == 4) {
            readLiteral(version, typeId, input)
        }
        else {
            val rem     = input.drop(6)
            val lTypeId = rem.head
            if (lTypeId == 0) {
                operatorByBits(version, typeId, rem.tail)
            }
            else {
                operatorByCount(version, typeId, rem.tail)
            }
        }
    }

    private def readLiteral(version: Long, typeId: Long, input: T): (Int, Literal) = {
        val app    = Iterator
            .iterate((6, List.empty[Long], 1))((read: Int, lst: List[Long], hd: Int) => {
                val rem  = input.drop(read)
                val nInt = parseBin(rem.drop(1).take(4))
                (read + 5, lst ::: nInt :: Nil, rem.head)
            })
            .dropWhile(_._3 == 1)
            .next()
        val fnlNum = app._2.foldLeft(0L)((a, b) => a * 10 + b)
        (app._1, new Literal(version, typeId, fnlNum))
    }

    private def operatorByCount(version: Long, typeId: Long, rem: T): (Int, Operator) = {
        val howManyToRead         = parseBin(rem.take(11));
        val (nowRead, subPackets) = Iterator
            .iterate((0, List.empty[HexPacket]))(readerNode(rem.drop(11)))
            .drop(howManyToRead.toInt)
            .next()
        (nowRead + 7 + 11, new Operator(version, typeId, subPackets))
    }

    private def operatorByBits(version: Long, typeId: Long, rem: T): (Int, Operator) = {
        //The next 15 bits denote how many bits to send through translate to get subpackets?
        val howManyBits           = parseBin(rem.take(15))
        val (nowRead, subPackets) = Iterator
            .iterate((0, List.empty[HexPacket]))(readerNode(rem.drop(15)))
            .dropWhile(_._1 < howManyBits)
            .next()
        (nowRead + 7 + 15, new Operator(version, typeId, subPackets))
    }

    private def readerNode(input: T)(read: Int, app: List[HexPacket]) = {
        val (additRead, nextPacket) = translate(input.drop(read))
        (read + additRead, app ::: nextPacket :: Nil)
    }

    def addVersions(hex: HexPacket): Long = {
        hex match {
            case Literal(version, _, _)           => version
            case Operator(version, _, subPackets) => version + subPackets.map(addVersions).sum
        }
    }

    def applyOperations(hex: HexPacket): Long = {
        hex match {
            case Literal(_, _, value)         => value
            case Operator(_, 0, subPackets) => subPackets.map(applyOperations).sum
            case Operator(_, 1, subPackets) => subPackets.map(applyOperations).product
            case Operator(_, 2, subPackets) => subPackets.map(applyOperations).min
            case Operator(_, 3, subPackets) => subPackets.map(applyOperations).max
            case Operator(_, 5, List(a, b)) => (if (applyOperations(a) > applyOperations(b)) {1L} else {0L})
            case Operator(_, 6, List(a, b)) => (if (applyOperations(a) < applyOperations(b)) {1L} else {0L})
            case Operator(_, 7, List(a, b)) => (if (applyOperations(a) == applyOperations(b)) {1L} else {0L})
            case Operator(_, _, _) => 0L
        }
    }

    def parseInput(): HexPacket = {
        translate(
        //   Source
        //       .fromResource("day16.txt")
        //       .getLines
        //       .toList
        //       .head
        "9C0141080250320F1802104A08".flatMap(hToB)
              .map(_.asDigit)
        )._2
    }

    def partOne(input: HexPacket): String = {
        addVersions(input).toString
    }

    def partTwo(input: HexPacket): String = {
        //16353679177 too low
        applyOperations(input).toString
    }
}
