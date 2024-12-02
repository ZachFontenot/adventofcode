package com.zf

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode

object Main extends IOApp {

  // This is your new "main"!
  def run(args: List[String]): IO[ExitCode] =
    args.headOption match {
      case Some(day) =>
        Advent.runDay(day.toInt).flatMap(IO.println).as(ExitCode.Success)
      case None =>
        IO(System.err.println("Usage: Advent day")).as(ExitCode(2))
    }
}
