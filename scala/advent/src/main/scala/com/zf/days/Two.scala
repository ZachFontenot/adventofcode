package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.Rfc5234.sp
import cats.parse.{Parser}

object Two extends DayType[List[List[Int]], Int] {
  def parseInput(inputLines: List[String]): List[List[Int]] = ???
  def partA(input: List[List[Int]]): Int = ???
  def partB(input: List[List[Int]]): Int = ???
}
