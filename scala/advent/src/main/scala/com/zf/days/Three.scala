package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.{Parser => P}
import cats.data.NonEmptyList

case class MulWrapper(countIt: Boolean, vals: List[MulInstr])
case class MulInstr(a: Int, b: Int)

object Three extends DayType[List[MulWrapper], Int] {
  def parseInput(inputLines: List[String]): List[MulWrapper] =
    val doP = P.string("do()")
    val dontP = P.string("don't()")
    val doOrDont = doP | dontP
    val mulParser: P[MulInstr] =
      ((P.string("mul(") *> intParser) ~ (P.char(',') *> intParser <* P.char(
        ')'
      ))).map { case (a, b) => MulInstr(a, b) }

    val p: P[MulInstr] = mulParser
      .between(
        P.anyChar.repUntil0(mulParser | doOrDont),
        P.anyChar.repUntil0(doOrDont | mulParser)
      )

    def mulWrap(yes: Boolean = true, until: P[Unit]): P[MulWrapper] = p
      .repUntil(until)
      .map { (nlst: NonEmptyList[MulInstr]) =>
        MulWrapper(yes, nlst.toList)
      }

    val yes = doP *> mulWrap(true, dontP)
    val no = dontP *> mulWrap(false, doP)
    val all = (mulWrap(true, doOrDont) ~ (yes | no).rep0)
    val parsed = all.parseAll(inputLines.flatten.mkString)
    val results: (MulWrapper, List[MulWrapper]) = parsed.getOrElse(
      (MulWrapper(false, List.empty), List(MulWrapper(false, List.empty)))
    )
    results._2.concat(List(results._1))

  def partA(input: List[MulWrapper]): Int =
    input.foldLeft(0) { case (acc, MulWrapper(_, lst)) =>
      lst.foldLeft(acc) { case (acc1, MulInstr(a, b)) => acc1 + (a * b) }
    }

  def partB(input: List[MulWrapper]): Int =
    input.foldLeft(0) { case (acc, MulWrapper(countIt, lst)) =>
      if countIt
      then
        acc + lst.foldLeft(0) { case (acc1, MulInstr(a, b)) => acc1 + (a * b) }
      else acc
    }
}
