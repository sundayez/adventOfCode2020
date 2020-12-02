package com.bernar.adventofcode2020

object Day2 extends App {
  val lines = FileInput.readStringFromFile("input2.txt")
  val regex = "(\\d+)-(\\d+) (\\w): ([a-z]+)".r
  println(lines.count(isPasswordPart1))
  println(lines.count(isPasswordPart2))

  private def isPasswordPart1(line: String) = line match {
    case regex(low, high, character, word) =>
      val count = word.count(_ == character.charAt(0))
      count >= low.toInt && count <= high.toInt
  }

  private def isPasswordPart2(line: String) = line match {
    case regex(pos1, pos2, character, word) =>
      word.charAt(pos1.toInt - 1) == character.charAt(0) ^ word.charAt(pos2.toInt - 1) == character.charAt(0)
  }

}


