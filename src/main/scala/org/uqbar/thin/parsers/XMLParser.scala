package org.uqbar.thin.parsers

import scala.util.parsing.combinator._

object XMLParser extends XMLParser
trait XMLParser extends RegexParsers {

  trait Element
  case class Node(id: String, attrb: List[Attribute], children: List[Element]) extends Element
  case class Leaf(id: String, attrb: List[Attribute], body: String) extends Element
  case class Attribute(id: String, value: String)

  def matchTags(id: String, id2: String) = (id == id2)
  def openingTag = tagDefiner("", (identifier) ~ (attribute.*))
  def closingTag = tagDefiner("/", identifier)
  def tagDefiner[U](closer: String, body: Parser[U]) = (("<" ++ closer) ~> body <~ ">")

  def xml: Parser[Element] = node | leaf
  def node = openingTag ~ xml.+ ~ closingTag ^^ { case id ~ attribs ~ children ~ id2 if matchTags(id, id2) => Node(id, attribs, children) }
  def leaf = leafWithBody // | leafNoBody
  def leafWithBody = (openingTag ~ body.? ~ closingTag) ^^ { case id ~ attribs ~ body ~ id2 if matchTags(id, id2) => Leaf(id, attribs, body.getOrElse(Nil).mkString(" ")) }
  // def leafNoBody = closingTagNoBody ^^ {case id ~ attribs => Leaf("id",attribs,"") }
  def attribute = identifier ~ "=" ~ string ^^ { case id ~ _ ~ value => Attribute(id, value) }
  def identifier = """[A-Za-z]+""".r ^^ { r => r.toString() }
  def string = """\w+""".r ^^ { r => r.toString() }
  def body = string.+

  def apply(input: String) = parseAll(xml.*, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _)  => throw ParseException(msg)
  }

}