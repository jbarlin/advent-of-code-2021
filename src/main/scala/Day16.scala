import lib.DayTemplate
import scala.io.Source
import scala.util.Try

abstract trait HexPacket

final case class Literal(val v: Int, val i: Int, val value: Long)                  extends HexPacket
final case class Operator(val v: Int, val i: Int, val subPackets: List[HexPacket]) extends HexPacket

type T = Seq[Int]

object Day16 extends DayTemplate[T] {

    private def parseBin(in: Seq[Int]): Int = {
        Integer.parseInt(in.foldLeft(0)((a, b) => a * 10 + b).toString, 2)
    }

    val hToB = Map(
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

    def translate(input: T): (Int, HexPacket) = {
        val version = parseBin(input.slice(0, 3))
        val typeId  = parseBin(input.slice(3, 6))
        val rem     = input.drop(6)
        if (typeId == 4) {
            val (lit, read) = makeLiteral(version, typeId, rem)
            (read, lit)
        }
        else {
            val lTypeId = rem.head
            if (lTypeId == 0) {
                //The next 15 bits denote how many bits to send through translate to get subpackets?
                val howManyBits           = parseBin(rem.tail.take(15)) + 16
                val (nowRead, subPackets) = Iterator
                    .iterate((16, List.empty[HexPacket]))
                    ((read, app) => {
                        val (additRead, nextPacket) = translate(rem.drop(read))
                        (read + additRead, app ::: nextPacket :: Nil)
                    })
                    .dropWhile(b => rem.size > b._1 + 8)
                    .dropWhile(_._1 < howManyBits)
                    .next()
                (nowRead, new Operator(version, typeId, subPackets))
            }
            else {
                val howManyToRead = parseBin(rem.tail.take(11)) + 12;
                val (nowRead, subPackets) = Iterator.iterate((12, List.empty[HexPacket]))
                    ((read, app) => {
                        val (additRead, nextPacket) = translate(rem.drop(read))
                        (read + additRead, app ::: nextPacket :: Nil)
                    })
                    .dropWhile(b => rem.size > b._1 + 8)
                    .drop(howManyToRead)
                    .next()
                (nowRead, new Operator(version, typeId, subPackets))
            }
        }

    }

    private def makeLiteral(
        v: Int,
        i: Int,
        input: T,
        acc: List[Int] = List.empty,
        bitsRead: Int = 6
    ): (Literal, Int) = {
        val myNum      = parseBin(input.slice(1, 6))
        val myBitsRead = bitsRead + 5
        if (input.head != 1) {
            //OK, read out the last chunk!
            val digits       = (acc ::: myNum :: Nil).foldLeft(0L)((a, b) => a * 10 + b)
            val literal      = new Literal(v, i, digits)
            val myNextHexEnd = myBitsRead % 4
            (literal, myNextHexEnd)
        }
        else {
            makeLiteral(v, i, input.drop(5), (acc ::: myNum :: Nil), myBitsRead)
        }
    }

    def addVersions(hex: HexPacket): Long = {
        hex match{
            case Literal(version, _, _) => version.toLong
            case Operator(version, _, subPackets) => version + subPackets.map(addVersions).sum
        }
    }

    def parseInput(): T = {
        Source
            .fromResource("day16-test-a.txt")
            .getLines
            .toList
            .head
            .flatMap(hToB)
            .map(_.asDigit)
    }

    def partOne(input: T): String = {
        val myHex = translate(input)._2
        addVersions(myHex).toString
    }

    def partTwo(input: T): String = {
        ""
    }
}
