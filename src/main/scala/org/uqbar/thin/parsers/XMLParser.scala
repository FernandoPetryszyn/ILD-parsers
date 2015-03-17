package org.uqbar.thin.parsers

import scala.util.parsing.combinator._

object XMLParser extends XMLParser
trait XMLParser extends RegexParsers {

  trait Element
  case class Node(id: String, attrb: List[Attribute], children: List[Element]) extends Element
  case class Leaf(id: String, attrb: List[Attribute], body: String) extends Element
  case class Attribute(id: String, value: String) //TODO to be extended

  def tagsWithBody[U](body: Parser[U]) = "<" ~> (identifier) >> { id => ((attribute.*) <~ ">") ~ body ~ ("</" ~> id <~ ">") }
  
  lazy val xml: Parser[Element] = leaf | node
  lazy val leaf = leafWithBody // | leafNoBody  
  lazy val node = tagsWithBody(xml.+) ^^ { case attribs ~ children ~ id => Node(id, attribs, children) }
  lazy val leafWithBody = tagsWithBody(body.?) ^^ { case attribs ~ body ~ id => Leaf(id, attribs, body.getOrElse(Nil).mkString(" ")) }
  lazy val attribute = identifier ~ "=" ~ string ^^ { case id ~ _ ~ value => Attribute(id, value) }
  lazy val identifier = """[A-Za-z]+""".r ^^ { r => r.toString() }
  lazy val string = """\w+""".r ^^ { r => r.toString() }
  lazy val body = string.+

  def apply(input: String) = parseAll(xml.*, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _)  => throw ParseException(msg)
  }

}