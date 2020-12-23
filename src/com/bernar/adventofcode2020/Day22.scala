package com.bernar.adventofcode2020

import scala.annotation.tailrec

object Day22 extends App {
  val input = FileInput.readStringFromFile("input22.txt")
  val players = input.split(sys.props("line.separator").repeat(2)).map(playerFromString).toList

  def playerFromString(playerString: String): Player = {
    val lines = playerString.split(sys.props("line.separator")).toList
    Player(lines.head.replace("Player ", "").replace(":", "").toInt, lines.tail.map(_.toInt))
  }

  //Part 1
  //  println("Final points: " + playRound1(NormalStatus(players.head, players(1)), 1))

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
  var lastGameNumber = 1
  var usedGameNumbers = List(lastGameNumber)
  println("Final points: " + calculateFinalPoints(playGame(NormalStatus(players.head, players(1)), 1)))

  def playGame(roundStatus: RoundStatus, gameNumber: Int): Player = {
    println(s"=== Game $gameNumber ===")
    val winner = playRound2(roundStatus, 1, gameNumber, List())
    println()
    if (usedGameNumbers.size > 1) {
      usedGameNumbers = usedGameNumbers.take(usedGameNumbers.size - 1)
      println(s"...anyway, back to game ${usedGameNumbers.last}")
    }
    winner
  }

  @tailrec
  def playRound2(roundStatus: RoundStatus, roundNumber: Int, gameNumber: Int, historic: List[RoundStatus]): Player = {
    roundStatus match {
      case NormalStatus(p1, p2) =>
        if (historic.contains(roundStatus)) {
          println(s"Repetition in game $gameNumber round $roundNumber")
          println(p1)
          println(p2)
          p1
        }
        else if (p1.cards.isEmpty) {
          println(s"The winner of game $gameNumber is player ${p2.id}")
          p2
        }
        else if (p2.cards.isEmpty) {
          println(s"The winner of game $gameNumber is player ${p1.id}")
          p1
        }
        else {
          println(s"-- Round $roundNumber (Game $gameNumber)--")
          println(p1)
          println(p2)
          println(s"Player ${p1.id} plays: ${p1.cards.head}")
          println(s"Player ${p2.id} plays: ${p2.cards.head}")
          if (p1.cards.head <= p1.cards.tail.size && p2.cards.head <= p2.cards.tail.size) {
            println("Playing a sub-game to determine the winner...")
            lastGameNumber = lastGameNumber + 1
            usedGameNumbers = usedGameNumbers :+ lastGameNumber
            val winner = playGame(NormalStatus(Player(p1.id, p1.cards.tail.take(p1.cards.head)), Player(p2.id, p2.cards.tail.take(p2.cards.head))), lastGameNumber)
            if (winner.id == p1.id) {
              println(s"Player 1 wins round $roundNumber of game $gameNumber!")
              playRound2(NormalStatus(Player(p1.id, p1.cards.tail ++ List(p1.cards.head, p2.cards.head)), Player(p2.id, p2.cards.tail)), roundNumber + 1, gameNumber, historic :+ roundStatus)
            } else {
              println(s"Player 2 wins round $roundNumber of game $gameNumber!")
              playRound2(NormalStatus(Player(p1.id, p1.cards.tail), Player(p2.id, p2.cards.tail ++ List(p2.cards.head, p1.cards.head))), roundNumber + 1, gameNumber, historic :+ roundStatus)
            }
          } else if (p1.cards.head > p2.cards.head) {
            println(s"Player 1 wins round $roundNumber of game $gameNumber!")
            playRound2(NormalStatus(Player(p1.id, p1.cards.tail ++ List(p1.cards.head, p2.cards.head)), Player(p2.id, p2.cards.tail)), roundNumber + 1, gameNumber, historic :+ roundStatus)
          } else {
            println(s"Player 2 wins round $roundNumber of game $gameNumber!")
            playRound2(NormalStatus(Player(p1.id, p1.cards.tail), Player(p2.id, p2.cards.tail ++ List(p2.cards.head, p1.cards.head))), roundNumber + 1, gameNumber, historic :+ roundStatus)

          }
        }
    }
  }


  def calculateFinalPoints(player: Player): Int = player.cards.zip(Range(player.cards.size, 0, -1))
    .map(pair => pair._1 * pair._2)
    .sum

  case class Player(id: Int, cards: List[Int]) {
    override def toString: String = s"Player $id's deck: ${cards.mkString(", ")}"

//    override def equals(obj: Any): Boolean = obj match {
//      case player: Player => id.equals(player.id) && cards.size == player.cards.size && cards.indices.forall(i => cards(i) == player.cards(i))
//    }
  }

  trait RoundStatus

  case class NormalStatus(p1: Player, p2: Player) extends RoundStatus //{
//    override def equals(obj: Any): Boolean =
//      obj match {
//        case status: NormalStatus => p1.equals(status.p1) && p2.equals(status.p2)
//        case _ => false
//      }
//  }

  case class FinalStatus(finalPoints: Int) extends RoundStatus

}

