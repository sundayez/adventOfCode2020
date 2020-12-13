package com.bernar.adventofcode2020

object Day11 extends App {
  val grid = FileInput.readLinesFromFile("input11.txt").map(s => s.toCharArray)
  grid.map(l => l.mkString).foreach(println)
  println(grid.size, grid.head.length)
  val grid2 = grid.clone

  grid.indices.foreach(i => grid(i).indices.foreach(j => updateSit(i, j)))

  private def updateSit(i: Int, j: Int): Unit = {
    grid(i)(j) match {
      case 'L' => if (allAdjacentsAreNoOccupied(i, j)) grid2(i)(j) = '#'
    }
  }

  private def allAdjacentsAreNoOccupied(i: Int, i1: Int): Boolean = ???
}


}

