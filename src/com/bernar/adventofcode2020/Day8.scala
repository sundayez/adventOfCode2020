package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day8 extends App {
  val lines = FileInput.readLinesFromFile("input8.txt")
  val instructions = lines.map(line => {
    val split = line.split(" ")
    split(0) match {
      case "acc" => Accumulate(split(1).toInt)
      case "nop" => Nop(split(1).toInt)
      case "jmp" => Jump(split(1).toInt)
    }
  })

  val instructionMap = instructions.indices.map(i => (i, instructions(i))).toMap

  //Part 1
  println(updateAccumulator(0, 0, instructionMap))

  //Part 2
  println(fixAndRun(instructionMap))

  private def fixAndRun(instructionMap: Map[Int, Instruction]): Int = {
    val indices = instructionMap.keys.filter(i => finishes(0, changePosition(i, instructionMap)))
    indices.map(index => updateAccumulator(0, 0, changePosition(index, instructionMap))).toList.head
  }

  private def changePosition(i: Int, instructionMap: Map[Int, Instruction]): Map[Int, Instruction] = {
    val changed = instructionMap(i) match {
      case Accumulate(operator, visited) => Accumulate(operator, visited)
      case Jump(operator, visited) => Nop(operator, visited)
      case Nop(operator, visited) => Jump(operator, visited)
    }
    instructionMap + (i -> changed)
  }

  @tailrec
  private def updateAccumulator(i: Int, accumulator: Int, instructionMap: Map[Int, Instruction]): Int = instructionMap(i) match {
    case Accumulate(_, true) => accumulator
    case Nop(_, true) => accumulator
    case Jump(_, true) => accumulator
    case Accumulate(operator, false) => updateAccumulator(i + 1, accumulator + operator, instructionMap + (i -> updateInstruction(instructionMap(i))))
    case Jump(operator, false) => updateAccumulator(i + operator, accumulator, instructionMap + (i -> updateInstruction(instructionMap(i))))
    case Nop(_, false) => updateAccumulator(i + 1, accumulator, instructionMap + (i -> updateInstruction(instructionMap(i))))
  }


  @tailrec
  private def finishes(i: Int, instructionMap: Map[Int, Instruction]): Boolean = instructionMap(i) match {
    case Accumulate(_, true) => false
    case Nop(_, true) => false
    case Jump(_, true) => false
    case Accumulate(_, false) => if (i == instructionMap.size - 1) true else finishes(i + 1, instructionMap + (i -> updateInstruction(instructionMap(i))))
    case Jump(operator, false) =>
      finishes(i + operator, instructionMap + (i -> updateInstruction(instructionMap(i))))
    case Nop(_, false) => if (i == instructionMap.size - 1) true else finishes(i + 1, instructionMap + (i -> updateInstruction(instructionMap(i))))
  }

  private def updateInstruction(instruction: Instruction): Instruction = instruction match {
    case Accumulate(operator, _) => Accumulate(operator, visited = true)
    case Jump(operator, _) => Jump(operator, visited = true)
    case Nop(operator, _) => Nop(operator, visited = true)
  }

  trait Instruction

  case class Accumulate(operator: Int, visited: Boolean = false) extends Instruction

  case class Nop(operator: Int, visited: Boolean = false) extends Instruction

  case class Jump(operator: Int, visited: Boolean = false) extends Instruction

}

