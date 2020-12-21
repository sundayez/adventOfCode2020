package com.bernar.adventofcode2020

object Day20 extends App {
  val input = FileInput.readStringFromFile("input20.txt")
  val tiles = input.split("\n".repeat(2)).map(tileFromString).toList
  tiles.foreach(println)

  def tileFromString(tileString: String): Tile = {
    val lines = tileString.split("\n").toList
    Tile(lines.head.replace("Tile ", "").replace(":", "").toInt, lines.tail.map(s => s.toCharArray))
  }

  case class Tile(index: Int, content: List[Array[Char]]) {
    override def toString: String = {
      val readableList = content.map(l => l.mkString).mkString("\n")
      s"Tile: $index\n$readableList\n"
    }
  }

  object Rotation extends Enumeration {
    val R0, R90, R180, R270 = Value
  }

  object Flip extends Enumeration {
    val NO, YES = Value
  }

}

