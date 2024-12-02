package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.Rfc5234.sp
import cats.parse.{Parser}

object One extends DayType[(List[Int], List[Int]), Int] {
  def parseInput(inputLines: List[String]): (List[Int], List[Int]) =
    val lineParser: Parser[(Int, Int)] = (intParser <* sp.rep) ~ intParser
    val output = inputLines.map { line =>
      lineParser.parse(line) match {
        case Right(parsed) => parsed(1)
        case Left(err)     => (0, 0)
      }
    }

    output.unzip

  def partA(input: (List[Int], List[Int])): Int =
    val list1 = input._1
    val list2 = input._2
    val summed = (list1.sorted
      .lazyZip(list2.sorted))
      .map((x: Int, y: Int) => (x - y).abs)
      .foldLeft(0)(_ + _)
    summed

  def partB(input: (List[Int], List[Int])): Int =
    val occurMap: Map[Int, Int] = input._2.groupBy(identity).view.mapValues(_.size).toMap
    input._1.map((num) => num * occurMap.getOrElse(num, 0)).foldLeft(0)(_ + _)
}
