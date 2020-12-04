package com.bernar.adventofcode2020

import scala.io.Source

object FileInput {

  def readLinesFromFile(fileName: String): List[String] = Source.fromResource(fileName).getLines.toList

  def readStringFromFile(fileName: String): String = Source.fromResource(fileName).mkString

}
