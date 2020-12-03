package com.bernar.adventofcode2020

object Day3 extends App {
  val lines = FileInput.readStringFromFile("input3.txt")
  val w = lines.head.length
  val h = lines.length

  println(countTrees(3, 1))
  println("=====")
  println(countTrees(1, 2))

  private def countTrees(hStep: Int, wStep: Int): Int = {
    println(w, h)
    println(Range(0, h, wStep).toList)
    println(Range(0, hStep*h, hStep).map(e => e % w).toList)
    println(Range(0, h, wStep).length)
    println(Range(0, hStep*h, hStep).map(e => e % w).length)
    println(Range(0, h).zip(Range(0, 3*h, 3).map(e => e % w)))

    Range(0, h).zip(Range(0, hStep*h, 3).map(e => e % w)).count(
      r => lines(r._1)(r._2) == '#'
    )
  }
}


