package com.bernar.adventofcode2020

object Day6 extends App {
  val fileInput = FileInput.readStringFromFile("input6.txt")
  val customAnswerGroups = fileInput.split("\n\n")

  //Part 1
  val totalAnswers = customAnswerGroups
    .map(s => s.replaceAll("\n", "").distinct.sorted.length)
    .sum

  println(totalAnswers)

  //Part 2

  val splitAnswers = customAnswerGroups
    .map(s => s.split("\n"))
    .map(group => group.reduce((a, b) => a.filter(b.contains(_))))
    .map(s => s.length)
    .sum
  println(splitAnswers)
}

