package com.bernar.adventofcode2020

import scala.util.matching.Regex

object Day4 extends App {
  val fileInput = FileInput.readStringFromFile("input4.txt")
  val passports1 = fileInput.split(sys.props("line.separator").repeat(2))
    .map(passportString => new PassportBuilder1(passportString).fromString1)
    .toList

  println(passports1.count(isPassportValid1))

  val passports2 = fileInput.split(sys.props("line.separator").repeat(2))
    .map(passportString => new PassportBuilder2(passportString).fromString2)
    .toList

  println(passports2.count(isPassportValid2))

  def isPassportValid1(passport: Passport): Boolean = {
    val output = passport.byr.isDefined &&
      passport.iyr.isDefined &&
      passport.eyr.isDefined &&
      passport.hgt.isDefined &&
      passport.hcl.isDefined &&
      passport.ecl.isDefined &&
      passport.pid.isDefined
    //println(passport + " " + output)
    output
  }


  def isPassportValid2(passport: Passport): Boolean = {
    val output = passport.byr.isDefined &&
      passport.iyr.isDefined &&
      passport.eyr.isDefined &&
      passport.hgt.isDefined &&
      passport.hcl.isDefined &&
      passport.ecl.isDefined &&
      passport.pid.isDefined
    //    println(passport + " " + output)
    output
  }

  abstract class PassportBuilder(val passportString: String) {

    protected def byrRegex: Regex

    protected def iyrRegex: Regex

    protected def eyrRegex: Regex

    protected def hgtRegex: Regex

    protected def hclRegex: Regex

    protected def eclRegex: Regex

    protected def pidRegex: Regex

    protected def cidRegex: Regex

    def fromString1: Passport = Passport(
      applyRegex1(byrRegex, _ => true), applyRegex1(iyrRegex, _ => true), applyRegex1(eyrRegex, _ => true), applyRegex1(hgtRegex, _ => true),
      applyRegex1(hclRegex, _ => true), applyRegex1(eclRegex, _ => true), applyRegex1(pidRegex, _ => true), applyRegex1(cidRegex, _ => true))

    def fromString2: Passport = Passport(
      applyRegex1(byrRegex, s => s.toInt >= 1920 && s.toInt <= 2002),
      applyRegex1(iyrRegex, s => s.toInt >= 2010 && s.toInt <= 2020),
      applyRegex1(eyrRegex, s => s.toInt >= 2020 && s.toInt <= 2030),
      applyRegex2(hgtRegex, (value, unit) => (unit == "cm" && value.toInt >= 150 && value.toInt <= 193) ||
        (unit == "in" && value.toInt >= 59 && value.toInt <= 76)),
      applyRegex1(hclRegex, _ => true),
      applyRegex1(eclRegex, s => s == "amb" || s == "blu" || s == "brn" || s == "gry" || s == "grn" || s == "hzl" || s == "oth"),
      applyRegex1(pidRegex, _.length <= 9),
      applyRegex1(cidRegex, _ => true))


    protected def applyRegex1(regex: Regex, condition: String => Boolean): Option[String] =
      regex.findFirstIn(passportString).getOrElse("") match {
        case regex(value) => Some(value).filter(condition)
        case _ => None
      }

    protected def applyRegex2(regex: Regex, condition: (String, String) => Boolean): Option[String] =
      regex.findFirstIn(passportString).getOrElse("") match {
        case regex(value, unit) => if (condition(value, unit)) Some(value + unit) else None
        case _ => None
      }
  }

  class PassportBuilder1(override val passportString: String) extends PassportBuilder(passportString) {
    override def byrRegex: Regex = "byr:([^\\s]+)".r

    override def iyrRegex: Regex = "iyr:([^\\s]+)".r

    override def eyrRegex: Regex = "eyr:([^\\s]+)".r

    override def hgtRegex: Regex = "hgt:([^\\s]+)".r

    override def hclRegex: Regex = "hcl:([^\\s]+)".r

    override def eclRegex: Regex = "ecl:([^\\s]+)".r

    override def pidRegex: Regex = "pid:([^\\s]+)".r

    override def cidRegex: Regex = "cid:([^\\s]+)".r
  }

  class PassportBuilder2(override val passportString: String) extends PassportBuilder(passportString) {
    override def byrRegex: Regex = "byr:(\\d+)".r

    override def iyrRegex: Regex = "iyr:(\\d+)".r

    override def eyrRegex: Regex = "eyr:(\\d+)".r

    override def hgtRegex: Regex = "hgt:(\\d+)(cm|in)".r

    override def hclRegex: Regex = "hcl:(#[0-9a-f]+)".r

    override def eclRegex: Regex = "ecl:([a-z]+)".r

    override def pidRegex: Regex = "pid:(\\d+)".r

    override def cidRegex: Regex = "cid:([^\\s]+)".r
  }

  case class Passport(byr: Option[String],
                      iyr: Option[String],
                      eyr: Option[String],
                      hgt: Option[String],
                      hcl: Option[String],
                      ecl: Option[String],
                      pid: Option[String],
                      cid: Option[String])

}

