package com.zf

object AdventHelpers {
  case class Pair(x: Int, y: Int) {
    def distance(other: Pair) =
      math.sqrt(math.pow((x - other.x), 2) + math.pow((y - other.y), 2))

    def add(p: Pair) =
      Pair(x + p.x, y + p.y)

    def sub(p: Pair) =
      Pair(x - p.x, y - p.y)

    def diff(p: Pair) =
      val s = this.sub(p)
      Pair(s.x.abs, s.y.abs)

    def order(p1: Pair, p2: Pair) =
      (p1.x >= p2.x, p1.y >= p2.y)

    def fromTuple(p: (Int, Int)) =
      Pair(p(0), p(1))

    def getNeighbors() =
      List((0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1))
        .map: p =>
          fromTuple(p)
  }  
}
