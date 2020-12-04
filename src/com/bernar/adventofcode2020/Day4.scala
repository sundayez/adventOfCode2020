package com.bernar.adventofcode2020

object Day4 extends App {
  val fileInput = FileInput.readStringFromFile("input4.txt")
  val blocks = fileInput.split("\n\n").toList
  println(fileInput)

  case class Passport(byr: String, iyr: String, eyr: String, hgt: String, hcl: String, ecl: String, pid: String, cid: String) {
    private val byrRegex = "byr:(\\w+)".r
    private val iyrRegex = "iyr:(\\w+)".r
    private val eyrRegex = "eyr:(\\w+)".r
    private val hgtRegex = "hgt:(\\w+)".r
    private val hclRegex = "hcl:(\\w+)".r
    private val eclRegex = "ecl:(\\w+)".r
    private val pidRegex = "pid:(\\w+)".r
    private val cidRegex = "cid:(\\w+)".r

    def fromString(passportString: String): String = //Passport(
      passportString match {
        case byrRegex(_, value) => value
        case _ => null
      }
//    )

  }

}

