package com.zf.days

import com.zf.DayType
import com.zf.AdventHelpers.{Pair}

object Eight extends DayType[Map[Pair, Char], Int] {
  val tester = List(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )

  def parseInput(inputLines: List[String]): Map[Pair, Char] =
    inputLines.zipWithIndex.foldLeft(Map.empty) { case (n, (str, i)) =>
      str.zipWithIndex.foldLeft(n) { case (m, (ch, j)) =>
        m.updated(Pair(i, j), ch)
      }
    }

  def findAntinodes(input: Map[Pair, Char], dupes: Int): Set[Pair] =
    val edgeMax = input.foldLeft(0) { case (acc, (Pair(a, b), _)) =>
      Math.max(acc, a)
    }
    input
      .groupMap(_._2)(_._1)
      .filter: (k, v) =>
        v.size >= 2 && k != '.'
      .map { (_, v) =>
        v.toList
          .combinations(2)
          .toList
      }
      .foldLeft(Set.empty[Pair]) { case (s, ps) =>
        ps.foldLeft(s): (antinodes, ps) =>
          val (p1, p2) = (ps(0), ps(1))
          val diff = p1.diff(p2)          
          val (a1, a2) = getDiffs(p1, p2, diff)          
          val (p1s, p2s) =
            (applyDiffsTil(p1, a1, dupes), applyDiffsTil(p2, a2, dupes))
          val toAdd =
            if dupes > 1
            then List(List(p1), List(p2), p1s, p2s)
            else List(p1s, p2s)
          antinodes ++ toAdd.flatten
        .filter { case (Pair(a, b)) =>
            0 <= a && a <= edgeMax && 0 <= b && b <= edgeMax
          }
      }

  def partA(input: Map[Pair, Char]): Int =
    findAntinodes(input, 1).size

  def applyDiffsTil(p: Pair, diff: Pair, max: Int): List[Pair] =
    val initList = p.add(diff) // basically doWhile
    Range(0, max - 1)
      .scanLeft(initList) { case (ls, _) =>
        ls.add(diff)
      }
      .toList

  def getDiffs(p1: Pair, p2: Pair, diff: Pair) =
    (p1.x - p2.x, p1.y - p2.y) match {
      // p1 x is less than p2 x, but y is greater
      case (a, b) if a <= 0 && b >= 0 =>
        (Pair(-diff.x, diff.y), Pair(diff.x, -diff.y))
      case (a, b) if a <= 0 && b <= 0 =>
        (Pair(-diff.x, -diff.y), Pair(diff.x, diff.y))
      case (a, b) if a >= 0 && b <= 0 =>
        (Pair(diff.x, -diff.y), Pair(-diff.x, diff.y))
      case _ =>
        (Pair(diff.x, diff.y), Pair(-diff.x, -diff.y))
    }

  def partB(input: Map[Pair, Char]): Int =
    findAntinodes(input, 100).size
}
