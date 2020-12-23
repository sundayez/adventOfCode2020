package com.bernar.adventofcode2020

object Day23 extends App {
  val input = 716892543
  println(CyclicCrab(input).cycle)

  case class CyclicCrab(input: Int) {
    var cycle: List[Int] = Range(1, 10).map(i => input / Math.pow(10, 9 - i).toInt % 10).toList
  }

}

