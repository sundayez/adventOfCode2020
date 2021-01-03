package com.bernar.adventofcode2020

object Day23 extends App {
  //val input = 716892543
  val input = 389125467
  val cyclicCrab = CyclicCrab(input)
  cyclicCrab.step(1)
  cyclicCrab.step(2)
  cyclicCrab.step(3)
  cyclicCrab.step(4)
  cyclicCrab.step(5)
  cyclicCrab.step(6)

  case class CyclicCrab(input: Int) {
    var cycle: Array[Int] = Range(1, 10).map(i => input / Math.pow(10, 9 - i).toInt % 10).toArray
    var currentPosition = 0

    def step(i: Int): Unit = {
      val pickup = pickUp()
      val destinationValue = destination(pickup)
      println(s"-- move $i --")
      println("cups: " + Range(0, 9).map(i => if (i == currentPosition) s"(${cycle(i)})" else cycle(i)).mkString(" "))
      println("pick up: " + pickup.mkString(", "))
      println("destination: " + destinationValue)
      val resulting = shiftUntilDestination(currentPosition + 4, destinationValue)
      cycle(resulting) = pickup.head
      cycle(resulting + 1) = pickup(1)
      cycle(resulting + 2) = pickup(2)
      currentPosition = currentPosition + 1
    }

    private def pickUp() = cycle.slice(currentPosition + 1, currentPosition + 4)

    private def destination(pickup: Array[Int]) = {
      var candidate = if (cycle(currentPosition) == 1) 9 else cycle(currentPosition) - 1
      while (pickup.contains(candidate)) {
        candidate = if (candidate == 1) 9 else candidate - 1
      }
      candidate
    }

    private def shiftUntilDestination(i: Int, destinationValue: Int): Int = {
      val moved = cycle(circularIndex(i))
      cycle(circularIndex(i - 3)) = moved
      if (moved != destinationValue) shiftUntilDestination(i + 1, destinationValue)
      else i - 2
    }

    private def circularIndex(i: Int) = if (i >= 0 && i < 9) i else Math.floorMod(i, 9)
  }

}

