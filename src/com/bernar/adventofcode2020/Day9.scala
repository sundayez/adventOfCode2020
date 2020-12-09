package com.bernar.adventofcode2020

object Day9 extends App {
  val numbers = FileInput.readLinesFromFile("input9.txt").map(s => s.toLong)

  val filtered = Range(25, numbers.size).filter(i => !isSumOfTwoPrevious(numbers(i), numbers.slice(i - 25, i))).head

  //Part 1
  val result1 = numbers(filtered)
  println(result1)

  //Part 2

  val rightRange = Range(2,numbers.size).map(i => {
    getIndexSubsetsOfFixedLength(i, numbers.size).filter(pairIndices => rangeSums(pairIndices, result1))
  }).filter(_.nonEmpty).head.head
  println(numbers.slice(rightRange._1, rightRange._2).min + numbers.slice(rightRange._1, rightRange._2).max)

  private def isSumOfTwoPrevious(number: Long, candidates: List[Long]): Boolean =
    Range(0, 25).flatMap(i => Range(i + 1, 25).map(j => (i, j))).exists(pair => number == candidates(pair._1) + candidates(pair._2))

  private def getIndexSubsetsOfFixedLength(length: Int, maxLength: Int): Seq[(Int, Int)] =
    Range(0, maxLength - length + 1).map(i => (i, i + length))

  private def rangeSums(sliceIndices: (Int, Int), result: Long): Boolean = numbers.slice(sliceIndices._1, sliceIndices._2).sum == result

}

