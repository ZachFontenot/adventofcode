package com.zf.days

import com.zf.DayType
import com.zf.ParseHelpers
import cats.parse.{Parser => P}
import com.zf.ParseHelpers.intParser

case class Update(ups: List[Int])
case class PrintOrder(before: Int, after: Int)
case class ManPage(
    before: Map[Int, List[Int]],
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
    orders.foldLeft(ManPage(Map.empty, Map.empty, updates)) {
      case (ManPage(b, a, us), PrintOrder(before, after)) =>
        val newB = b.updatedWith(after): v =>
          v match {
            case Some(lst) => Some(before :: lst)
            case None      => Some(List(before))
          }
        val newA = a.updatedWith(before): v =>
          v match {
            case Some(lst) => Some(after :: lst)
            case None      => Some(List(after))
          }
        ManPage(newB, newA, us)
    }

  def validOrdering(
      update: List[Int],
      befores: Map[Int, List[Int]],
      afters: Map[Int, List[Int]]
  ): Boolean =
    val withIdx = update.zip(Range(0, update.size))
    withIdx.forall { case (toCheck, idx) =>
      val a = afters.getOrElse(toCheck, List.empty)
      val b = befores.getOrElse(toCheck, List.empty)

      withIdx.take(idx).forall { case (n, _idx) =>
        b.contains(n) || !a.contains(n)
      } &&
      withIdx.drop(idx + 1).forall { case (n, _idx) =>
        a.contains(n) || !b.contains(n)
      }

    }

  def getMiddle[A](ls: List[A]): A =
    ls(ls.size / 2)

  def partA(input: ManPage): Int =
    val ManPage(befores, afters, updates) = input
    updates.foldLeft(0) { case (acc, Update(update)) =>
      if validOrdering(update, befores, afters)
      then acc + getMiddle(update)
      else acc
    }

  def sortUpdate(
      update: List[Int],
      befores: Map[Int, List[Int]],
      afters: Map[Int, List[Int]]
  ): List[Int] =
    val withIdx = update.zip(Range(0, update.size))

    withIdx
      .sortWith { case ((a, aidx), (b, bidx)) =>
        val afts = afters.getOrElse(a, List.empty)
        val befs = befores.getOrElse(a, List.empty)
        if !befs.contains(b) && !afts.contains(b) then aidx < bidx
        else
          val bidxs = befs.zip(Range(0, befs.size)).toMap
          val curIdx = befs.size
          val aftidxs =
            afts.zip(Range(befs.size + 1, befs.size + 1 + afts.size)).toMap
          curIdx < bidxs.getOrElse(b, aftidxs.getOrElse(b, 100))
      }
      .map { case (n, _) => n }

  def partB(input: ManPage): Int =
    val ManPage(befores, afters, updates) = input
    val incorrect = updates.filter { case Update(update) =>
      !validOrdering(update, befores, afters)
    }
    incorrect.foldLeft(0) { case (acc, Update(update)) =>
      acc + getMiddle(sortUpdate(update, befores, afters))
    }
}
