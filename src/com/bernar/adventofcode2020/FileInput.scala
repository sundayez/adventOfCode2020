package com.bernar.adventofcode2020

import scala.io.Source

object FileInput {

  def readStringFromFile(fileName: String): List[String] = Source.fromResource(fileName).getLines.toList

}
