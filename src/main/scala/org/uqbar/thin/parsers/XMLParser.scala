package org.uqbar.thin.parsers

import scala.util.parsing.combinator._

object XMLParser extends XMLParser
trait XMLParser extends RegexParsers {
  
  trait Element
  case class Node(id: String,attrb: List[Attribute],children: List[Element]) extends Element
  case class Leaf(id: String,attrb: List[Attribute],body: String) extends Element
  case class Attribute(id: String,value:String)
  
  //Por ahora los valores son solo strings, podria extenderse para distinguir algunas cosas
  //NO estoy teniendo en cuenta aun los tags que se cierran solos (i.e </tag>)
  def matchTags(id: String, id2: String) = (id == id2)
  
  
  def xml:Parser[Element] = node | leaf
  def node = ("<" ~> (identifier) ~ attribute.* <~ ">") ~ xml.+ ~ ("</" ~> identifier <~ ">") ^^ { case id ~ attribs ~ children ~ id2 if matchTags(id,id2) => Node(id,attribs,children)}
  def attribute = identifier ~ "=" ~ string ^^ {case id ~ _ ~ value => Attribute(id,value)}
  def identifier = """[A-Za-z]+""".r ^^ {r => r.toString()}
  def leaf = ("<" ~> (identifier) ~ attribute.* <~ ">") ~ body.? ~ ("</" ~> identifier <~ ">") ^^ { case id ~ attribs ~ body ~ id2 if matchTags(id,id2)=> Leaf(id,attribs,body.getOrElse(Nil).mkString(" "))  }
  def string = """\w+""".r ^^ { r => r.toString()}
  def body = string.+
  
  //peque;a simplificacion en el segundo pazo
	def apply(input: String) = parseAll(xml.*, input) match {
		case Success(result, _) => result
		case NoSuccess(msg, _) => throw ParseException(msg)
	}
  
  
}