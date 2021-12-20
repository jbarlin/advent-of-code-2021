package lib.day16

final case class Operator(val v: Long, val i: Long, val subPackets: List[HexPacket]) extends HexPacket
