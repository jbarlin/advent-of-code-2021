package lib.day3

final class Tally(val zeros: Int = 0, val ones: Int = 0) {
    def add(digit: Int): Tally = {
        if digit == 1 then {
            Tally(this.zeros, this.ones + 1)
        }
        else {
            Tally(this.zeros + 1, this.ones)
        }
    }

    def +(that: Tally) = {
        Tally(that.zeros + this.zeros, that.ones + this.ones)
    }

    def isEqual(): Boolean = {
        this.ones == this.zeros
    }

    def toLarger(): Int = {
        if this.ones > this.zeros then {
            1
        }
        else {
            0
        }
    }

    def toSmaller(): Int = {
        if this.ones < this.zeros then {
            1
        }
        else {
            0
        }
    }
}
