package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers._
import cats.parse.{Parser => P}
import cats.data.NonEmptyList
import cats.parse.Parser0

case class MulWrapper(countIt: Boolean, vals: List[MulInstr])
case class MulInstr(a: Int, b: Int)

object Three extends DayType[List[MulWrapper], Int] {
  def parseInput(inputLines: List[String]): List[MulWrapper] =
    val doP = P.string("do()")
    val dontP = P.string("don't()")
    val doOrDont = doP | dontP
    // capture mul(<d>,<d>) and map it to the MulInstruction case
    // class
    val mulParser: P[MulInstr] =
      ((P.string("mul(") *> intParser) ~ (P.char(',') *> intParser <* P.char(
        ')'
      ))).map { case (a, b) => MulInstr(a, b) }
    // This will capture all cases of mul(<d>,<d>) by being repeated
    // at the Until points
    val p: P[MulInstr] = mulParser
      .between(
        P.anyChar.repUntil0(mulParser | doOrDont),
        P.anyChar.repUntil0(doOrDont | mulParser)
      )
    // collect results into wrapper until encounter opposite
    // instruction via do/don't instructions, function to handle the
    // flop and flip
    def mulWrap(yes: Boolean = true, until: P[Unit]): P[MulWrapper] = p
      .repUntil(until)
      .map { (nlst: NonEmptyList[MulInstr]) =>
        MulWrapper(yes, nlst.toList)
      }
    // do() then collect muls until encounter don't()
    val yes = doP *> mulWrap(true, dontP)
    // vice versa
    val no = dontP *> mulWrap(false, doP)
    // collect first muls before other instructions then repeat the above parsers
    val all: Parser0[List[MulWrapper]] =
      (mulWrap(true, doOrDont) | yes | no).rep0

    val parsed = all.parseAll(inputLines.flatten.mkString)
    val results: List[MulWrapper] = parsed.getOrElse(
      List(MulWrapper(false, List.empty))
    )
    results

  def partA(input: List[MulWrapper]): Int =
    input.foldLeft(0) { case (acc, MulWrapper(_, lst)) =>
      lst.foldLeft(acc) { case (acc1, MulInstr(a, b)) => acc1 + (a * b) }
    }

  def partB(input: List[MulWrapper]): Int =
    input.foldLeft(0) { case (acc, MulWrapper(countIt, lst)) =>
      if countIt
      then lst.foldLeft(acc) { case (acc1, MulInstr(a, b)) => acc1 + (a * b) }
      else acc
    }
}
