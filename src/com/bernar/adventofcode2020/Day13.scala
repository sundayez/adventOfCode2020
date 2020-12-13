package com.bernar.adventofcode2020

object Day13 extends App {
  val input = FileInput.readLinesFromFile("input13.txt")
  val arrivalTime = input.head.toInt
  val busLines = input(1).split(",").filter(s => !s.equals("x")).map(s => s.toInt)
  val firstBusArrivals = busLines.map(line => (line, arrivalTime + line - arrivalTime % line))
  val lineAndWaitingTime = firstBusArrivals.map(pair => (pair._1, pair._2 - arrivalTime))

  //Part 1
  println(lineAndWaitingTime.minBy(_._2)._1 * lineAndWaitingTime.minBy(_._2)._2)
}

