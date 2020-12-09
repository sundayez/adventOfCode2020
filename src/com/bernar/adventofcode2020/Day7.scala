package com.bernar.adventofcode2020

object Day7 extends App {
  val lines = FileInput.readLinesFromFile("input7.txt")
  val leftSide = "(.*)\\sbags contain\\s"
  val rightSideEmpty = "no other bags."
  val rightSideMiddle = "(\\d)*\\s(.)*bags|bag,\\s"
  val rightSideEnd = "(\\d)*\\s(.)*bags|bag."
  val regexEmpty = "(.*)\\sbags contain\\sno other bags.".r
  val regexStandard = "(.*)\\sbags contain\\s(\\d+\\s.*bags|bag,\\s)*(\\d)*\\s(.)*bags|bag\\.".r
  lines.foreach(line => {
    line match {
      case regexEmpty(leftBag) => println(line + ": " + leftBag.trim)
      case regexStandard(leftBag, array, rightNumber, alsfsdf) => println(s"$line: $leftBag, $array, $rightNumber, $alsfsdf")
      case _ => "no match"
    }
  })
//  val rules = lines.map(buildRule)


  private def buildRule(line: String): Rule = {
    val split1 = line.split("contain")
    val leftBags = Bags(split1(0).split("bags")(0).trim, 1)
    val right = split1(1).split(",")
    Rule(Bags("", 1), List(Bags("", 1)))
  }

  case class Bags(bagType: String, quantity: Int)

  // case class NoBags() extends Bags("", 0)

  case class Rule(container: Bags, contained: List[Bags])

}

