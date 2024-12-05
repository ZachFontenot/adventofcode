package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers
import cats.parse.{Parser => P}
import com.zf.ParseHelpers.intParser

case class Update(ups: List[Int])
case class PrintOrder(before: Int, after: Int)
case class ManPage(
    after: Map[Int, List[Int]],
    updates: List[Update]
)

object Five extends DayType[ManPage, Int] {
  val tester = List(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "\n",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  )
  def mapEithers[E, A](e: List[Either[E, A]]): List[A] = e.map {
    case Right(a) => a // this fine
  }

  def parseInput(inputLines: List[String]): ManPage =
    val oP = ((intParser <* P.char('|')) ~ intParser).map(PrintOrder.apply)
    val uP = (intParser <* P.char(',').?).rep0.map(Update.apply)
    val split = inputLines.groupBy: line =>
      (line.contains("|"), line.contains(",")) match {
        case (true, false)  => "|"
        case (false, true)  => ","
        case (false, false) => "newline"
        case (true, true)   => "base"
      }
    val orders = mapEithers(split.getOrElse("|", List.empty).map(oP.parseAll))
    val updates = mapEithers(split.getOrElse(",", List.empty).map(uP.parseAll))
    orders.foldLeft(ManPage(Map.empty, updates)) {
      case (ManPage(a, us), PrintOrder(before, after)) =>
        val newA = a.updatedWith(before): v =>
          v match {
            case Some(lst) => Some(after :: lst)
            case None      => Some(List(after))
          }
        ManPage(newA, us)
    }

  def sortPrinter(
      afters: Map[Int, List[Int]],
      update: List[Int]
  ) =
    update.sortWith { case (a, b) =>
      afters.getOrElse(a, List.empty).contains(b)
    }

  def getMiddle[A](ls: List[A]): A =
    ls(ls.size / 2)

  def validOrder(update: List[Int], f: List[Int] => List[Int]) =
    f(update) == update

  def partA(input: ManPage): Int =
    val ManPage(afters, updates) = input
    val f = (a: List[Int]) => sortPrinter(afters, a)
    updates.foldLeft(0) { case (acc, Update(update)) =>
      if validOrder(update, f)
      then acc + getMiddle(update)
      else acc
    }

  def partB(input: ManPage): Int =
    val ManPage(afters, updates) = input
    val f = (a: List[Int]) => sortPrinter(afters, a)
    val incorrect = updates.filter { case Update(update) =>
      !validOrder(update, f)
    }
    incorrect.foldLeft(0) { case (acc, Update(update)) =>
      acc + getMiddle(f(update))
    }
}
