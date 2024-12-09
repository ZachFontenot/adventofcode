package com.zf.days

import com.zf.DayType
import cats.parse.{Parser => P}
import cats.parse.Rfc5234.sp
import com.zf.ParseHelpers._

val tester = List(
  "190: 10 19",
  "3267: 81 40 27",
  "83: 17 5",
  "156: 15 6",
  "7290: 6 8 6 15",
  "161011: 16 10 13",
  "192: 17 8 14",
  "21037: 9 7 18 13",
  "292: 11 6 16 20"
)

case class Equation(toSolve: Long, values: List[Long])
type Op = (Long, Long) => Long

object Seven extends DayType[List[Equation], Long] {
  def parseInput(inputLines: List[String]): List[Equation] =
    val eqParser: P[Equation] =
      ((longParser <* P.char(':')) ~ (sp *> longParser).rep0).map {
        case (s, vs) =>
          Equation(s, vs)
      }
    inputLines.map(eqParser.parseAll(_) match {
      case Right(eq) => eq
      case Left(err) => Equation(0, List.empty)
    })

  val ops: List[Op] =
    List(
      (a, b) => a + b,
      (a, b) => a * b,
      (a, b) => (a.toString + b.toString).toLong
    )

  def testEquations(
      ops: List[Op],
      currentVal: Long,
      targetVal: Long,
      rest: List[Long]
  ): Boolean =
    if currentVal > targetVal then false
    else
      rest match 
        case Nil => currentVal == targetVal
        case x :: xs =>
          ops
            .exists: op =>
              testEquations(
                ops,
                op(currentVal, x),
                targetVal,
                xs
              )      

  def partA(input: List[Equation]): Long =
    input.foldLeft(0L) { case (acc, Equation(toSolve, values)) =>
      if testEquations(ops.take(2), values.head, toSolve, values.tail)
      then acc + toSolve
      else acc
    }

  def partB(input: List[Equation]): Long =
    input.foldLeft(0L) { case (acc, Equation(toSolve, values)) =>
      if testEquations(ops, values.head, toSolve, values.tail)
      then acc + toSolve
      else acc
    }
}
