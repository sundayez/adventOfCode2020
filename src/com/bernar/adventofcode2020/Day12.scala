package com.bernar.adventofcode2020

object Day12 extends App {
  val movements = FileInput.readLinesFromFile("input12.txt").map(movementFromString)

  //Part 1
  val finalPosition1 = movements.foldLeft(Position(0, 0))((position, movement) => calculateNextPosition1(movement, position))
  println(finalPosition1)
  println(math.abs(finalPosition1.x) + math.abs(finalPosition1.y))

  //Part 2
  val finalPosition2 = movements.foldLeft(PositionWayPoint(0, 0, 10, 1))((positionWayPoint, movement) => calculateNextPosition2(movement, positionWayPoint))
  println(finalPosition2)
  println(math.abs(finalPosition2.x) + math.abs(finalPosition2.y))

  private def movementFromString(string: String): Movement = string.charAt(0) match {
    case 'N' => MoveNorth(string.substring(1).toInt)
    case 'S' => MoveSouth(string.substring(1).toInt)
    case 'E' => MoveEast(string.substring(1).toInt)
    case 'W' => MoveWest(string.substring(1).toInt)
    case 'L' => TurnLeft(string.substring(1).toInt)
    case 'R' => TurnRight(string.substring(1).toInt)
    case 'F' => MoveForward(string.substring(1).toInt)
  }

  private def calculateNextPosition1(movement: Movement, position: Position): Position = movement match {
    case MoveNorth(distance) => Position(position.x, position.y + distance, position.orientation)
    case MoveSouth(distance) => Position(position.x, position.y - distance, position.orientation)
    case MoveEast(distance) => Position(position.x + distance, position.y, position.orientation)
    case MoveWest(distance) => Position(position.x - distance, position.y, position.orientation)
    case TurnLeft(deg) => Position(position.x, position.y, position.orientation match {
      case Orientation.East => if (deg == 90) Orientation.North else if (deg == 180) Orientation.West else Orientation.South
      case Orientation.West => if (deg == 90) Orientation.South else if (deg == 180) Orientation.East else Orientation.North
      case Orientation.North => if (deg == 90) Orientation.West else if (deg == 180) Orientation.South else Orientation.East
      case Orientation.South => if (deg == 90) Orientation.East else if (deg == 180) Orientation.North else Orientation.West
    })
    case TurnRight(deg) => Position(position.x, position.y, position.orientation match {
      case Orientation.East => if (deg == 90) Orientation.South else if (deg == 180) Orientation.West else Orientation.North
      case Orientation.West => if (deg == 90) Orientation.North else if (deg == 180) Orientation.East else Orientation.South
      case Orientation.North => if (deg == 90) Orientation.East else if (deg == 180) Orientation.South else Orientation.West
      case Orientation.South => if (deg == 90) Orientation.West else if (deg == 180) Orientation.North else Orientation.East
    })
    case MoveForward(distance) => position.orientation match {
      case Orientation.East => Position(position.x + distance, position.y, position.orientation)
      case Orientation.West => Position(position.x - distance, position.y, position.orientation)
      case Orientation.North => Position(position.x, position.y + distance, position.orientation)
      case Orientation.South => Position(position.x, position.y - distance, position.orientation)
    }
  }

  private def calculateNextPosition2(movement: Movement, positionWayPoint: PositionWayPoint): PositionWayPoint = movement match {
    case MoveNorth(distance) => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.xWayPoint, positionWayPoint.yWayPoint + distance)
    case MoveSouth(distance) => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.xWayPoint, positionWayPoint.yWayPoint - distance)
    case MoveEast(distance) => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.xWayPoint + distance, positionWayPoint.yWayPoint)
    case MoveWest(distance) => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.xWayPoint - distance, positionWayPoint.yWayPoint)
    case TurnLeft(deg) => rotateLeft(positionWayPoint, deg)
    case TurnRight(deg) => rotateRight(positionWayPoint, deg)
    case MoveForward(distance) => PositionWayPoint(positionWayPoint.x + distance * positionWayPoint.xWayPoint, positionWayPoint.y + distance * positionWayPoint.yWayPoint,
      positionWayPoint.xWayPoint, positionWayPoint.yWayPoint)
  }

  private def rotateLeft(positionWayPoint: PositionWayPoint, deg: Int): PositionWayPoint = deg match {
    case 90 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, -positionWayPoint.yWayPoint, positionWayPoint.xWayPoint)
    case 180 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, -positionWayPoint.xWayPoint, -positionWayPoint.yWayPoint)
    case 270 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.yWayPoint, -positionWayPoint.xWayPoint)
  }

  private def rotateRight(positionWayPoint: PositionWayPoint, deg: Int): PositionWayPoint = deg match {
    case 90 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, positionWayPoint.yWayPoint, -positionWayPoint.xWayPoint)
    case 180 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, -positionWayPoint.xWayPoint, -positionWayPoint.yWayPoint)
    case 270 => PositionWayPoint(positionWayPoint.x, positionWayPoint.y, -positionWayPoint.yWayPoint, positionWayPoint.xWayPoint)
  }

  /*
  Counter clockwise:          Clockwise:
  x' = x cos phi - y sin phi  x' = x cos phi + y sin phi
  y' = x sin phi + y cos phi  y' = y cos phi - x sin phi

  phi = pi/2
  x' = -y                     x' = y
  y' = x                      y' = -x

  phi = pi
  x' = -x                     x' = -x
  y' = -y                     y' = -y

  phi = 3pi/2
  x' = y                      x' = -y
  y' = -x                     y' = x
   */
  trait Movement

  case class MoveNorth(distance: Int) extends Movement

  case class MoveSouth(distance: Int) extends Movement

  case class MoveEast(distance: Int) extends Movement

  case class MoveWest(distance: Int) extends Movement

  case class TurnLeft(deg: Int) extends Movement

  case class TurnRight(deg: Int) extends Movement

  case class MoveForward(distance: Int) extends Movement

  object Orientation extends Enumeration {
    val East, West, North, South = Value
  }

  case class Position(x: Int, y: Int, orientation: Orientation.Value = Orientation.East)

  case class PositionWayPoint(x: Int, y: Int, xWayPoint: Int, yWayPoint: Int)

}

