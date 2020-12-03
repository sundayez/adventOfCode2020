package com.bernar.adventofcode2020

object Day3 extends App {
  val lines = FileInput.readStringFromFile("input3.txt")
  val w = lines.head.length
  val h = lines.length

  println(countTrees(3, 1))
  println(countTrees(1, 1) * countTrees(3, 1) *
    countTrees(5, 1) * countTrees(7, 1) * countTrees(1, 2))

  private def countTrees(hStep: Int, wStep: Int): Int = {
    Range(0, h, wStep).zip(Range(0, hStep * h, hStep).map(e => e % w)).count(r => lines(r._1)(r._2) == '#')
  }
}

//    println(w, h)
//    println(Range(0, h, wStep).toList)
//    println(Range(0, hStep * h, hStep).map(e => e % w).toList)
//    println(Range(0, h, wStep).length)
//    println(Range(0, hStep * h, hStep).map(e => e % w).length)
//    println(Range(0, h, wStep).zip(Range(0, hStep * h, hStep).map(e => e % w)))
//    println(Range(0, h, wStep).zip(Range(0, hStep * h, hStep).map(e => e % w)).length)



