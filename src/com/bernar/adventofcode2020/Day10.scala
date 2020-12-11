package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day10 extends App {
  val numbers = FileInput.readLinesFromFile("input10.txt").map(s => s.toInt)
  val (threes, ones) = calculateIntervals(numbers).partition(_ == 3)

  //Part 1
  println(threes.size * ones.size)

  //Part 2
  var factorialMap = Map(1L -> 1L)

  val smallList1 = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4).sorted
  val smallList2 = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3).sorted

  val regex = "1+".r
  val intervals1 = calculateIntervals(smallList1).mkString("")
  val intervals2 = calculateIntervals(smallList2).mkString("")
  val intervalinput = calculateIntervals(numbers).mkString("")

  val result1 = (regex findAllIn intervals1).toList.map(s => s.length).filter(_ > 1).map(i => combinationsSequence(i + 1)).product
  val result2 = (regex findAllIn intervals2).toList.map(s => s.length).filter(_ > 1).map(i => combinationsSequence(i + 1)).product
  val resultinput = (regex findAllIn intervalinput).toList.map(s => s.length).filter(_ > 1).map(i => combinationsSequence(i + 1)).product


  println(result1)
  println(result2)
  println(resultinput)

  private def calculateIntervals(numbers: List[Int]) = {
    val max = numbers.max
    val chargers = (0 :: max + 3 :: numbers).sorted
    val differences = chargers
      .slice(0, chargers.size - 1)
      .zip(chargers.slice(1, chargers.size))
      .map(pair => pair._2 - pair._1)

    differences
  }

  def combinationsSequence(length: Int): Long = if (length == 2) 0 else if (length == 3) 2
  else {
    val n = length - 2
    Range(0, 3).map(i => comb(n, i)).sum + (if (n > 3) comb(n, 3) - (n - 2) else 0)

  }

  def fact(n: Long): Long = {
    if (factorialMap.contains(n)) factorialMap(n)
    else if (n == 0L)
      1L
    else {
      val result = n * factorialMap.getOrElse(n - 1, fact(n - 1))
      factorialMap = factorialMap + (n -> result)
      result
    }
  }

  @tailrec
  def comb(n: Long, m: Long): Long = if (m == 0) 1L
  else if (m > n / 2) comb(n, n - m)
  else fact(n) / (fact(m) * fact(n - m))

  /*
  secuencia 2 -> no interiores -> 0
  secuencia 3 -> 1 interior -> 2
  secuencia 4 -> 2 interiores -> maneras de quitar 0 + maneras de quitar 1 + maneras de quitar 2 -> (2,0) + (2,1) + (2,2) = 1 + 2 + 1 = 4
  secuencia 5 -> 3 interiores -> maneras de quitar 0 + maneras de quitar 1 + maneras de quitar 2 -> (3,0) + (3,1) + (3,2) = 1 + 3 + 3 = 7
  secuencia 6 -> 4 interiores -> maneras de quitar 0 + maneras de quitar 1 + maneras de quitar 2 + maneras de quitar 3 -> (4,0) + (4,1) + (4,2) + [(4,3) - (4-3+1)] = 1 + 4 + 6 + 2 = 13
  secuencia 7 -> 5 interiores -> (5,0) + (5,1) + (5,2) + [(5,3) - (5-3+1)] = 1 + 5 + 10 + 10 - 3 = 23

   */
  /*
  1,2,3,4,5,6,7
  1,3,4,5,6,7
  1,2,4,5,6,7
  1,2,3,5,6,7
  1,2,3,4,6,7
  1,2,3,4,5,7
  1,4,5,6,7
  1,3,5,6,7
  1,3,4,6,7
  1,3,4,5,7
  1,2,5,6,7
  1,2,4,6,7
  1,2,4,5,7
  1,2,3,6,7
  1,2,3,5,7
  1,2,3,4,7
  1,4,6,7
  1,4,5,7
  1,3,6,7
  1,3,5,7
  1,3,4,7
  1,2,5,7
  1,2,4,7
   */


  /*
  1,2,3,4,5
  1,3,4,5
  1,2,4,5
  1,2,3,5
  1,4,5
  1,3,5
  1,2,5
  */
  /*
  1,2,3,4,5,6
  1,3,4,5,6
  1,2,4,5,6
  1,2,3,5,6
  1,2,3,4,6
  1,4,5,6
  1,3,5,6
  1,3,4,6
  1,2,5,6
  1,2,4,6
  1,2,3,6
  1,4,6
  1,3,6
   */
}

