package com.bernar.adventofcode2020

object Day13 extends App {
  val input = FileInput.readLinesFromFile("input13.txt")
  val arrivalTime = input.head.toInt
  val busLinesAndIndices = input(1).split(",").indices.map(i => (i, input(1)(i)))
  val busLines = input(1).split(",").filter(s => !s.equals("x")).map(s => s.toInt)
  val firstBusArrivals = busLines.map(line => (line, arrivalTime + line - arrivalTime % line))
  val lineAndWaitingTime = firstBusArrivals.map(pair => (pair._1, pair._2 - arrivalTime))

  //Part 1
  println(lineAndWaitingTime.minBy(_._2)._1 * lineAndWaitingTime.minBy(_._2)._2)

  //Part 2
  /*
  7,13,x,x,59,x,31,19

  t = 0 mod 7
  t+1 = 0 mod 13
  t+4 = 0 mod 59
  t+6 = 0 mod 31
  t+7 = 0 mod 19
   */
}

