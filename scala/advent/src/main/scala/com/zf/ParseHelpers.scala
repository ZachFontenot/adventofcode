package com.zf

import cats.parse.{Parser => P, Numbers}

object ParseHelpers {
  def intParser: P[Int] = Numbers.digits.map(_.toInt)
  def longParser: P[Long] = Numbers.digits.map(_.toLong)
  def whitespace: P[Unit] = P.charIn(" \t\r\n").void
}
