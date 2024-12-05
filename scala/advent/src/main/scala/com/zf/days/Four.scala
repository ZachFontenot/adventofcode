package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.{Parser => P}
import cats.parse.Rfc5234.alpha

object Four extends DayType[Vector[Vector[Char]], Int] {
  enum Dir:
    case Up, Down, Left, Right, NW, NE, SW, SE

  val tester = List(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  )

  val sequence = List('M', 'A', 'S')
  // using the parser for the fun of it
  def parseInput(inputLines: List[String]): Vector[Vector[Char]] =
    inputLines.toVector.map: (line) =>
      alpha.rep0.parseAll(line) match {
        case Right(chars) => chars.toVector
        case Left(err)    => Vector.empty
      }

  def checkBounds(x: Int, y: Int, bnds: (Int, Int)): Boolean =
    Range(0, bnds._1).contains(x) && Range(0, bnds._2).contains(y)

  def getNeighborsInBounds(x: Int, y: Int, bnds: (Int, Int)) =
    val toAdd = List(
      Dir.NW,
      Dir.Up, 
      Dir.Left,
      Dir.SE,
      Dir.Down,
      Dir.Right,
      Dir.NE,
      Dir.SW, 
    ).map(dir => (dir, getDir(dir, x, y)))

    toAdd.filter { case (_, (x, y)) =>
      checkBounds(x, y, bnds)
    }

  def getDir(dir: Dir, x: Int, y: Int) =
    dir match {
      case Dir.NW    => (x - 1, y - 1)
      case Dir.NE    => (x - 1, y + 1)
      case Dir.SW    => (x + 1, y - 1)
      case Dir.SE    => (x + 1, y + 1)
      case Dir.Up    => (x - 1, y)
      case Dir.Down  => (x + 1, y)
      case Dir.Left  => (x, y - 1)
      case Dir.Right => (x, y + 1)
    }

  def findXmas(
      inp: Vector[Vector[Char]],
      startX: Int,
      startY: Int,
      bounds: (Int, Int),
      toFindIdx: Int
  ): Int =
    val toFind = sequence(toFindIdx)
    val toAdd = getNeighborsInBounds(startX, startY, bounds)
    val chars = toAdd.map { case (dir, (x, y)) =>
      (dir, (x, y), inp(x)(y))
    }
    val nxt = chars.filter { case (dir, coords, ch) =>
      ch == toFind // aka 'M'
    }

    nxt
      .map { case (dir, (x, y), _) =>
        followDir(inp, x, y, bounds, toFindIdx + 1, dir)
      }
      .foldLeft(0): (acc, n) =>
        acc + n

  def followDir(
      inp: Vector[Vector[Char]],
      coordsX: Int,
      coordsY: Int,
      bounds: (Int, Int),
      toFindIdx: Int,
      dir: Dir
  ): Int =
    if toFindIdx > sequence.size - 1 then 0
    else
      val toFind = sequence(toFindIdx)
      val (nxtX, nxtY) = getDir(dir, coordsX, coordsY)
      if checkBounds(nxtX, nxtY, bounds) then
        val char = inp(nxtX)(nxtY)
        if toFind == 'S' && char == 'S' then 1
        else if toFind == char then
          followDir(inp, nxtX, nxtY, bounds, toFindIdx + 1, dir)
        else 0
      else 0

  def findXmasV2(
      inp: Vector[Vector[Char]],
      startX: Int,
      startY: Int,
      bounds: (Int, Int)
  ): Int =
    val chars = getNeighborsInBounds(startX, startY, bounds)
      .filter { case (dir, _) =>
        List(Dir.NW, Dir.NE, Dir.SE, Dir.SW).contains(dir)
      }
      .map { case (_, (x, y)) =>
        inp(x)(y)
      }
    if List(
        List('M', 'S', 'S', 'M'),
        List('M', 'S', 'M', 'S'),
        List('S', 'M', 'M', 'S'),
        List('S', 'M', 'S', 'M')
      ).exists(_ == chars)
    then 1
    else 0

  def partA(input: Vector[Vector[Char]]): Int =
    var result = 0
    val bnds @ (xE, yE) = (140, 140)

    for ridx <- Range(0, xE) do {
      for cidx <- Range(0, yE) do {
        if input(ridx)(cidx) == 'X'
        then result += findXmas(input, ridx, cidx, bnds, 0)
      }
    }
    result

  def partB(input: Vector[Vector[Char]]): Int =
    var result = 0
    val bnds @ (xE, yE) = (input.size, input(0).size)

    for ridx <- Range(1, xE - 1) do {
      for cidx <- Range(1, yE - 1) do {
        if input(ridx)(cidx) == 'A' then
          result += findXmasV2(input, ridx, cidx, bnds)
      }
    }
    result
}
