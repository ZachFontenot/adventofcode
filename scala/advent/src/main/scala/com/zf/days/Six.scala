package com.zf.days

import com.zf.DayType
import scala.collection.mutable

val tester = Vector(
  "....#.....",
  ".........#",
  "..........",
  "..#.......",
  ".......#..",
  "..........",
  ".#..^.....",
  "........#.",
  "#.........",
  "......#..."
)

case class Coords(x: Int, y: Int) {
  def add(p: Coords): Coords =
    Coords(x + p.x, y + p.y)
  
  def turn(): Coords =
    (x, y) match {
      case (-1, 0) => Coords(0, 1)
      case (0, 1)  => Coords(1, 0)
      case (1, 0)  => Coords(0, -1)
      case (0, -1) => Coords(-1, 0)
    }
}

object Six extends DayType[Map[Coords, Char], Int] {
  def parseInput(inputLines: List[String]): Map[Coords, Char] =
    //inputLines.toVector
    //tester
    inputLines.zipWithIndex.foldLeft(Map.empty) {
      case (n, (str, i)) =>
        str.toVector.zipWithIndex.foldLeft(n) { case (m, (ch, j)) =>
          m.updated(Coords(i, j), ch)
        }
    }  

  def getStart(coordMap: Map[Coords, Char]): Coords =
    coordMap.find { case (crds, ch) => ch == '^' } match {
      case Some(coords, ch) => coords
      case None             => Coords(0, 0) // shouldn't happen
    }

  def guardWalk(
    coordMap: Map[Coords, Char],
    start: Coords,
    dir: Coords = Coords(-1, 0),
    seen: Set[Coords] = Set()
  ): Set[Coords] =    
    val next = start.add(dir)
    coordMap.getOrElse(next, '?') match {
      case '.' | '^' => guardWalk(coordMap, next, dir, seen + start)
      case '#' => guardWalk(coordMap, start, dir.turn(), seen)
      case '?' => seen + start
    }
   
  def partA(input: Map[Coords, Char]): Int =
    val visited = guardWalk(input, getStart(input))
    visited.size

  def doesLoop(
      mp: Map[Coords, Char],
      start: Coords,
      dir: Coords = Coords(-1, 0),
      seen: Set[(Coords, Coords)] = Set()
  ): Boolean =        
    seen.contains((start, dir)) match {
      case true =>        
        true
      case false =>
        val next = start.add(dir)        
        mp.getOrElse(next, '?') match {
          case '.' | '^' => doesLoop(mp, next, dir, seen + ((start, dir)))
          case '#' => doesLoop(mp, start, dir.turn(), seen)
          case '?' => false
        }
    }

  def partB(input: Map[Coords, Char]): Int =
    val guardSet = guardWalk(input, getStart(input))    
        
    guardSet.foldLeft(0) { case (acc, coords) =>      
      if doesLoop(input.updated(coords, '#'), getStart(input)) then
        acc + 1
      else acc
    }    
}
