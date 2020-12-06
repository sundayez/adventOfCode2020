package com.bernar.adventofcode2020

import scala.util.matching.Regex

object Day4 extends App {
  val fileInput = FileInput.readStringFromFile("input4.txt")
  val passportsPieces = fileInput.split(sys.props("line.separator").repeat(2))

  val splitPassportPieces = passportsPieces
    .map(piece => piece.split(" ").toList)
    .map(piece => piece.flatMap(subPiece => subPiece.split(sys.props("line.separator"))))
    .toList

  val passports = splitPassportPieces.map(buildPassport)

  //  println(passports.count(isPassportValid))
  println(passports.count(isPassportValid2))

  def isPassportValid(passport: Passport): Boolean = {
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
    val hgtRegex = "(\\d+)(cm|in)".r
    val hclRegex = "(#[0-9a-f]{6})".r

    val output = passport.byr.isDefined &&
      passport.byr.get.toInt >= 1920 &&
      passport.byr.get.toInt <= 2002 &&
      passport.iyr.isDefined &&
      passport.iyr.get.toInt >= 2010 &&
      passport.iyr.get.toInt <= 2020 &&
      passport.eyr.isDefined &&
      passport.eyr.get.toInt >= 2020 &&
      passport.eyr.get.toInt <= 2030 &&
      passport.hgt.isDefined && {
      passport.hgt.get match {
        case hgtRegex(value, unit) => (unit.equals("cm") && value.toInt >= 150 && value.toInt <= 193) ||
          (unit.equals("in") && value.toInt >= 59 && value.toInt <= 76)
        case _ => false
      }} &&
      passport.hcl.isDefined && {
      passport.hcl.get match {
        case hclRegex(_) => true
        case _ => false
      }} &&
      passport.ecl.isDefined && (passport.ecl.get.equals("amb") || passport.ecl.get.equals("blu") ||
      passport.ecl.get.equals("brn") || passport.ecl.get.equals("gry") || passport.ecl.get.equals("grn") || passport.ecl.get.equals("hzl") || passport.ecl.get.equals("oth")) &&
      passport.pid.isDefined && passport.pid.get.forall(_.isDigit) && passport.pid.get.length == 9
    println(passport + " " + output)
    output
  }

  case class Passport(byr: Option[String],
                      iyr: Option[String],
                      eyr: Option[String],
                      hgt: Option[String],
                      hcl: Option[String],
                      ecl: Option[String],
                      pid: Option[String],
                      cid: Option[String]) {
    override def toString: String = "Passport(byr: " + byr + " iyr: " + iyr + " eyr: " + eyr + " hgt: " + hgt +
      " hcl: " + hcl + " ecl: " + ecl + " pid: " + pid + " cid: " + cid + ")"
  }

  def buildPassport(passportPiece: List[String]): Passport = {
    var byr: Option[String] = None
    var iyr: Option[String] = None
    var eyr: Option[String] = None
    var hgt: Option[String] = None
    var hcl: Option[String] = None
    var ecl: Option[String] = None
    var pid: Option[String] = None
    var cid: Option[String] = None
    passportPiece.foreach(item => {
      if (item.startsWith("byr")) byr = Some(item.substring(4))
      if (item.startsWith("iyr")) iyr = Some(item.substring(4))
      if (item.startsWith("eyr")) eyr = Some(item.substring(4))
      if (item.startsWith("hgt")) hgt = Some(item.substring(4))
      if (item.startsWith("hcl")) hcl = Some(item.substring(4))
      if (item.startsWith("ecl")) ecl = Some(item.substring(4))
      if (item.startsWith("pid")) pid = Some(item.substring(4))
      if (item.startsWith("cid")) cid = Some(item.substring(4))
    })
    Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, cid)
  }

}

