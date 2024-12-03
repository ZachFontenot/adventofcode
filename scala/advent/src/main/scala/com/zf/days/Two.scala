package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.Rfc5234.sp
import cats.parse.{Parser}
import cats.data.NonEmptyList

object Two extends DayType[List[List[Int]], Int] {
  def parseInput(inputLines: List[String]): List[List[Int]] =
    val lineParser: Parser[NonEmptyList[Int]] = (intParser <* sp.?).rep
    val output = inputLines.map { line =>
      lineParser.parse(line) match {
        case Right(parsed) => parsed(1).toList
        case Left(err)     => List()
      }
    }

    output

  def isSafe(report: List[Int]): Boolean =
    val pairs = report.zip(report.tail).map((a, b) => b - a)
    val incOrDec = pairs.forall(_.sign == -1) || pairs.forall(_.sign == 1)
    val maxDiff = pairs.map(_.abs).max
    incOrDec && maxDiff <= 3 && maxDiff >= 1  

  def remove(num: Int, lst: List[Int]): List[Int] =
    lst diff List(num)

  def partA(input: List[List[Int]]): Int =
    input
      .map(isSafe)
      .count(identity)

  // Harder than I expected builtin combinations misbehaved on exactly
  // two cases using remove above failed on three cases but manually
  // stepping through each element and blotting it out worked
  def partB(input: List[List[Int]]): Int =
    input.count(report =>
      isSafe(report) ||
        Range(0, report.size).exists(i =>
          isSafe(report.take(i).concat(report.drop(i + 1)))
        )
    )
}
