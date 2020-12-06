package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day5 extends App {
  val lines = FileInput.readLinesFromFile("input5.txt")
  val seatNumbers = lines.map(getSeatNumber)
  val min = seatNumbers.min
  val max = seatNumbers.max

  //Part 1
  println(max)

  //Part 2
  println(Range(min, max).filter(!seatNumbers.contains(_)).head)

  @tailrec
  private def processCode(code: List[Char], min: Int, max: Int, minChar: Char, maxChar: Char): Int = code match {
    case Nil => min
    case head :: tail => processCode(tail, if (head == minChar) min else 1 + (min + max) / 2, if (head == maxChar) max else (min + max) / 2, minChar, maxChar)
  }

  private def getSeatNumber(key: String) = {
    val rowCode = key.substring(0, 7)
    val seatCode = key.substring(7, 10)
    processCode(rowCode.toList, 0, 127, 'F', 'B') * 8 + processCode(seatCode.toList, 0, 7, 'L', 'R')
  }
}

