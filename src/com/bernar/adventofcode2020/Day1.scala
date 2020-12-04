package com.bernar.adventofcode2020

object Day1 extends App {
  val numbers = FileInput.readLinesFromFile("input1.txt").map(s => s.toInt)
  println(calculateCombinations(2))
  println(calculateCombinations(3))


  private def calculateCombinations(elements: Int) = {
    numbers.combinations(elements).filter(_.sum == 2020).map(_.product).toList.head
  }
}
