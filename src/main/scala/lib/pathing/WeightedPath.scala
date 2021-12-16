package lib.pathing

final class WeightedPath[T](val from: T, val to: T, val weight: Double) {
    override def toString = "WeightedPath(" + from + "," + to + "," + weight + ")"
}

object WeightedPath{
    def apply[T](to: T, from: T, weight: Double): WeightedPath[T] = new WeightedPath(from, to, weight)
    def byTo[T](to: T, weight: Double)(from: T): WeightedPath[T] = new WeightedPath(from, to, weight)
}