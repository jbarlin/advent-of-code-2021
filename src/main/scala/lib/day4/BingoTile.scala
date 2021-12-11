package lib.day4

final class BingoTile(val value: Int, val used: Boolean = false) {
    def apply(valueLookup: Int): BingoTile = {
        return BingoTile(this.value, this.used || this.value == valueLookup)
    }
}
