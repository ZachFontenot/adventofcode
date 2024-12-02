package com.zf
trait DayType[Input, Output]:
  def parseInput(inputLines: List[String]): Input
  def partA(input: Input): Output
  def partB(input: Input): Output
