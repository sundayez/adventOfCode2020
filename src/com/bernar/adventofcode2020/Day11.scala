package com.bernar.adventofcode2020

object Day11 extends App {
  var grid = FileInput.readLinesFromFile("input11.txt").map(s => s.toCharArray)
  var equal = false

  grid.map(l => l.mkString).foreach(println)
  println(" ")
  while(!equal) {
    val grid2 = updateGrid(grid)
//    grid2.map(l => l.mkString).foreach(println)
//    println(" ")
    equal = areGridsEqual(grid, grid2)
    grid = copyGrid(grid2)
  }

  println(grid.map(row => row.count(_ == '#')).sum)

  private def areGridsEqual(grid1: List[Array[Char]], grid2: List[Array[Char]]): Boolean =
    grid1.indices.forall(
      i => grid1(i).indices.forall(
        j => grid1(i)(j) == grid2(i)(j)
      )
    )

  private def updateGrid(grid: List[Array[Char]]) = {
    var grid2 = List[Array[Char]]()
    grid.indices.foreach(i => {
      var currentArray = Array[Char]()
      grid(i).indices.foreach(j => {
        currentArray = currentArray :+ updatedSit(i, j)
      })
      grid2 = grid2 :+ currentArray
    })
    grid2
  }

  private def updatedSit(i: Int, j: Int): Char = grid(i)(j) match {
    case 'L' => if (allAdjacentsAreNoOccupied(grid, i, j)) '#' else 'L'
    case '.' => '.'
    case '#' => if (fourOrMoreAdjacentsAreOccupied(grid, i, j)) 'L' else '#'
  }


  private def buildAdjacentNeighbourhood(grid: List[Array[Char]], i: Int, j: Int) = Range(i - 1, i + 2).filter(idx => idx >= 0 && idx < grid.size)
    .flatMap(idx => Range(j - 1, j + 2).filter(jdx => jdx >= 0 && jdx < grid.head.length).map(jdx => (idx, jdx)).filter(indices => indices._1 != i || indices._2 != j))

  private def buildSeeLineNeighbourhood(grid: List[Array[Char]], i: Int, j: Int) = Range(i - 1, i + 2).filter(idx => idx >= 0 && idx < grid.size)
    .flatMap(idx => Range(j - 1, j + 2).filter(jdx => jdx >= 0 && jdx < grid.head.length).map(jdx => (idx, jdx)).filter(indices => indices._1 != i || indices._2 != j))

  private def allAdjacentsAreNoOccupied(grid: List[Array[Char]], i: Int, j: Int) = buildAdjacentNeighbourhood(grid, i, j).forall(index => grid(index._1)(index._2) != '#')

  private def fourOrMoreAdjacentsAreOccupied(grid: List[Array[Char]], i: Int, j: Int) = buildAdjacentNeighbourhood(grid, i, j).count(index => grid(index._1)(index._2) == '#') >= 4

  private def copyGrid(grid: List[Array[Char]]): List[Array[Char]] = {
    var grid2 = List[Array[Char]]()
    grid.indices.foreach(i => {
      var currentArray = Array[Char]()
      grid(i).indices.foreach(j => {
        currentArray = currentArray :+ grid(i)(j)
      })
      grid2 = grid2 :+ currentArray
    })
    grid2
  }

}

