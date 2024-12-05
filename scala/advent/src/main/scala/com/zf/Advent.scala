package com.zf

import java.io._
import cats.effect._
import scala.jdk.CollectionConverters._
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

  def runDay(day: Int): IO[String] =
    val mod = day match {
      case 1 => One
      case 2 => Two
      case 3 => Three
      case 4 => Four
      case 5 => Five
      case _ => One
    }
    loadFile(s"./inputs/day${day}.txt").flatMap { fileInput =>
      val input = mod.parseInput(fileInput)
      val partA = mod.partA(input)
      val partB = mod.partB(input)
      IO.pure(s"Part A: ${partA}\nPart B: ${partB}")
    }
}
