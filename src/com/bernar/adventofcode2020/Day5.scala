package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day5 extends App {
  val lines = FileInput.readLinesFromFile("input5.txt")
  val result = lines.map(line => {
    val rowCode = line.substring(0, 7)
    val seatCode = line.substring(7, 10)
    processRowCode(rowCode.toList, 0, 127) * 8 + processSeatCode(seatCode.toList, 0, 7)
  }).max

  println(result)

  @tailrec
  def processRowCode(rowCode: List[Char], min: Int, max: Int): Int = rowCode match {
    case Nil => min
    case head :: tail => processRowCode(tail, if (head == 'F') min else 1 + (min + max) / 2, if (head == 'B') max else (min + max) / 2)
  }

  @tailrec
  def processSeatCode(seatCode: List[Char], min: Int, max: Int): Int = seatCode match {
    case Nil => min
    case head :: tail => processSeatCode(tail, if (head == 'L') min else 1 + (min + max) / 2, if (head == 'R') max else (min + max) / 2)
  }
}

