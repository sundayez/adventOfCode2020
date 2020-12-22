package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day22 extends App {
  val input = FileInput.readStringFromFile("input22.txt")
  val players = input.split("\n".repeat(2)).map(playerFromString).toList

  def playerFromString(playerString: String): Player = {
    val lines = playerString.split("\n").toList
    Player(lines.head.replace("Player ", "").replace(":", "").toInt, lines.tail.map(_.toInt))
  }

  //Part 1
  println("Final points: " + playRound1(NormalStatus(players.head, players(1)), 1))

  @tailrec
  def playRound1(roundStatus: RoundStatus, roundNumber: Int): Int = {
    roundStatus match {
      case FinalStatus(finalPoints) => finalPoints
      case NormalStatus(p1, p2) => {
        if (p1.cards.isEmpty) {
          println("== Post-game results ==")
          println(p1)
          println(p2)
          playRound1(FinalStatus(calculateFinalPoints(p2)), roundNumber + 1)
        }
        else if (p2.cards.isEmpty) {
          println("== Post-game results ==")
          println(p1)
          println(p2)
          playRound1(FinalStatus(calculateFinalPoints(p1)), roundNumber + 1)
        }
        else {
          println(s"-- Round $roundNumber --")
          println(p1)
          println(p2)
          println(s"Player ${p1.id} plays: ${p1.cards.head}")
          println(s"Player ${p2.id} plays: ${p2.cards.head}")
          if (p1.cards.head > p2.cards.head) {
            println("Player 1 wins the round!")
            playRound1(NormalStatus(Player(p1.id, p1.cards.tail ++ List(p1.cards.head, p2.cards.head)), Player(p2.id, p2.cards.tail)), roundNumber + 1)
          } else {
            println("Player 2 wins the round!")
            playRound1(NormalStatus(Player(p1.id, p1.cards.tail), Player(p2.id, p2.cards.tail ++ List(p2.cards.head, p1.cards.head))), roundNumber + 1)

          }
        }
      }
    }
  }

  //Part 2
  println("Final points: " + playGame(NormalStatus(players.head, players(1)), 1, 1))

  def playGame(roundStatus: RoundStatus, roundNumber: Int, gameNumber: Int): Int = {
    println(s"=== Game $gameNumber ===")
    playRound2(roundStatus, 1, gameNumber)
  }

  def playRound2(roundStatus: RoundStatus, roundNumber: Int, gameNumber: Int): Int = {
    roundStatus match {
      case FinalStatus(finalPoints) => finalPoints
      case NormalStatus(p1, p2) => {
        if (p1.cards.isEmpty) {
          println("== Post-game results ==")
          println(p1)
          println(p2)
          playRound1(FinalStatus(calculateFinalPoints(p2)), roundNumber + 1)
        }
        else if (p2.cards.isEmpty) {
          println("== Post-game results ==")
          println(p1)
          println(p2)
          playRound1(FinalStatus(calculateFinalPoints(p1)), roundNumber + 1)
        }
        else {
          println(s"-- Round $roundNumber --")
          println(p1)
          println(p2)
          println(s"Player ${p1.id} plays: ${p1.cards.head}")
          println(s"Player ${p2.id} plays: ${p2.cards.head}")
          if (p1.cards.head <= p1.cards.tail.size && p2.cards.head <= p2.cards.tail.size) {

          }
          if (p1.cards.head > p2.cards.head) {
            println("Player 1 wins the round!")
            playRound1(NormalStatus(Player(p1.id, p1.cards.tail ++ List(p1.cards.head, p2.cards.head)), Player(p2.id, p2.cards.tail)), roundNumber + 1)
          } else {
            println("Player 2 wins the round!")
            playRound1(NormalStatus(Player(p1.id, p1.cards.tail), Player(p2.id, p2.cards.tail ++ List(p2.cards.head, p1.cards.head))), roundNumber + 1)

          }
        }
      }
    }
  }


  def calculateFinalPoints(player: Player): Int = player.cards.zip(Range(player.cards.size, 0, -1))
    .map(pair => pair._1 * pair._2)
    .sum

  case class Player(id: Int, cards: List[Int]) {
    override def toString: String = s"Player $id's deck: ${cards.mkString(", ")}"
  }

  trait RoundStatus

  case class NormalStatus(p1: Player, p2: Player) extends RoundStatus

  case class FinalStatus(finalPoints: Int) extends RoundStatus

}

