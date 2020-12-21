package com.bernar.adventofcode2020

object Day19 extends App {
  val input = FileInput.readStringFromFile("input19.txt")
  val rulesAndPasswords = input.split("\n".repeat(2))
  val rulesString = rulesAndPasswords.head.split("\n")
  val rules = rulesString.indices.map(i => (i, makeRuleFromLine(rulesString(i)))).toMap
  rules.foreach(println)
  val resolvedRules: Map[Int, List[String]] = Map()
//  println(rules(0).resolve(resolvedRules))
  val passwords = rulesAndPasswords(1).split("\n")

  def makeRuleFromLine(ruleString: String): Rule = {
    val leftRight = ruleString.split(":").map(_.trim)
    val index = leftRight.head.toInt
    val splitRight = leftRight(1).split("\\|").map(_.trim)
    if (splitRight.length == 1 && (splitRight.head == "\"a\"" || splitRight.head == "\"b\"")) charRule(index, splitRight.head.charAt(1))
    else if (splitRight.length == 1) makeRuleFromConcat(splitRight.head)
    else orRule(makeRuleFromConcat(splitRight.head), makeRuleFromConcat(splitRight(1)))
  }

  def makeRuleFromConcat(ruleConcat: String): Rule = {
    val split = ruleConcat.split(" ")
    if (split.length == 1) unresolvedClause(split.head.toInt) else concatRule(unresolvedClause(split.head.toInt), makeRuleFromConcat(split.tail.mkString(" ")))
  }

  trait Rule {
    def resolve(resolvedRules: Map[Int, List[String]]): (List[String], Map[Int, List[String]])
  }

  trait Clause

  case class unresolvedClause(index: Int) extends Rule {
    override def resolve(resolvedRules: Map[Int, List[String]]): (List[String], Map[Int, List[String]]) = if (resolvedRules.contains(index)) (resolvedRules(index), resolvedRules) else {
      val resolution = rules(index).resolve(resolvedRules)._1
      (resolution, resolvedRules + (index -> resolution))
    }
  }

  case class charRule(index: Int, char: Char) extends Rule {
    override def resolve(resolvedRules: Map[Int, List[String]]): (List[String], Map[Int, List[String]]) = if (resolvedRules.contains(index)) (resolvedRules(index), resolvedRules) else {
      (List(char.toString), resolvedRules)
    }
  }

  case class concatRule(left: Rule, right: Rule) extends Rule {
    override def resolve(resolvedRules: Map[Int, List[String]]): (List[String], Map[Int, List[String]]) =
      (for {l <- left.resolve(resolvedRules)._1; r <- right.resolve(resolvedRules)._1} yield l + r, resolvedRules)
  }

  case class orRule(left: Rule, right: Rule) extends Rule {
    override def resolve(resolvedRules: Map[Int, List[String]]): (List[String], Map[Int, List[String]]) =
      (left.resolve(resolvedRules)._1 ++ right.resolve(resolvedRules)._1, resolvedRules)
  }

}

