package com.bernar.adventofcode2020

object Day18 extends App {
  val input = FileInput.readLinesFromFile("input18.txt")
  val expressions = input.map(line => Expression(line.replaceAll(" ", "")))
  val operation1 = S(M(S(M(S(N(1), N(2)), N(3)), N(4)), N(5)), N(6))
  val operation2 = S(S(N(1), M(N(2), N(3))), M(N(4), S(N(5), N(6))))
  println(operation1.eval)
  println(operation2.eval)
//  println(expressions.head.splitByLeftMostSubexpression)

  trait Operation {
    def eval: Int
  }

  case class N(digit: Int) extends Operation {
    override def eval: Int = digit
  }

  case class S(left: Operation, right: Operation) extends Operation {
    override def eval: Int = left.eval + right.eval
  }

  case class M(left: Operation, right: Operation) extends Operation {
    override def eval: Int = left.eval * right.eval
  }

  case class Expression(expr: String) {
//    def splitByLeftMostSubexpression: (String, Operator.Value, Expression) = expr.head match {
//      case digitRegex(digit, operator) => (digit, if (operator == "+") Operator.Sum else Operator.Mult, Expression(expr.substring(2)))
//      case _ => ("a", Operator.Sum, this)
//    }
  }

  object Operator extends Enumeration {
    val Sum, Mult = Value
  }

}

