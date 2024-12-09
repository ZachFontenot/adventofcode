package com.zf

import java.io._
import cats.effect._
import com.zf.days._

object Advent {
  def loadFile(path: String): IO[List[String]] =
    val readerResource: Resource[IO, BufferedReader] =
      Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(path))))

    readerResource.use { reader =>
      def readLines(acc: List[String]): IO[List[String]] =
        IO(Option(reader.readLine())).flatMap {
          case Some(line) => readLines(acc :+ line)
          case None       => IO.pure(acc)
        }
      readLines(Nil)
    }

  val implemented = 8

  def runAll(): List[IO[String]] =
    for {
      day <- Range.inclusive(1, implemented).toList
    } yield runDay(day)

  def runDay(day: Int): IO[String] =
    val mod = day match {
      case 1 => One
      case 2 => Two
      case 3 => Three
      case 4 => Four
      case 5 => Five
      case 6 => Six
      case 7 => Seven
      case 8 => Eight
      case _ => One
    }

    for {
      (fileTime, fileInput) <- loadFile(s"./inputs/day${day}.txt").timed
      (timeP, input) <- IO.pure(mod.parseInput(fileInput)).timed
      (timeA, resultA) <- IO.pure(mod.partA(input)).timed
      (timeB, resultB) <- IO.pure(mod.partB(input)).timed
    } yield s"""
Runing Day $day\n
File Reading: ${fileTime.toMillis} ms; Parsing: ${timeP}\n
Part A: ${resultA}; completed in ${timeA}\n
Part B: ${resultB}; completed in ${timeB}"""
}
