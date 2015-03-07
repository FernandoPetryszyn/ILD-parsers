package org.uqbar.thin.parsers

import scala.util.parsing.combinator._

object MathParser extends MathParser

trait MathParser extends RegexParsers {
  trait Exp
  
  case class N(value: Int) extends Exp
  case class Add(l: Exp, r: Exp) extends Exp
  case class Sub(l: Exp, r: Exp) extends Exp
  case class Div(l: Exp, r: Exp) extends Exp
  case class Mul(l: Exp, r: Exp) extends Exp
  
  
  def exp: Parser[Exp] = number | add
  def factor: Parser[Exp] = number | "(" ~> exp <~ ")"
  def term: Parser[Exp] = div | mul | number
  def number = "-?[0-9]+".r ^^ { n => N(n.toInt) }
  def add = term ~ ("+" ~ exp).? ~ factor ^^ { case l ~ _ ~ r => Add(l, r) }
  def sub = term ~ ("-" ~ exp).? ~ factor ^^ { case l ~ _ ~ r => Sub(l, r) }
  def div = factor ~ "/" ~ term ^^ { case l ~ _ ~ r => Div(l, r) }
  def mul = factor ~ "*" ~ term ^^ { case l ~ _ ~ r => Mul(l, r) }
  
  
  def apply(input: String) = parseAll(???, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _)  => throw ParseException(msg)
  }
}