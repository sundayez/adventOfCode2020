package com.bernar.adventofcode2020

object Day9 extends App {
  val numbers = FileInput.readLinesFromFile("input9.txt").map(s => s.toLong)

  val filtered = Range(25, numbers.size).filter(i => !isSumOfTwoPrevious(numbers(i), numbers.slice(i - 25, i))).head

  //Part 1
  println(numbers(filtered))

  private def isSumOfTwoPrevious(number: Long, candidates: List[Long]): Boolean =
    Range(0, 25).flatMap(i => Range(i+1, 25).map(j => (i,j))).exists(pair => number == candidates(pair._1) + candidates(pair._2))

}

