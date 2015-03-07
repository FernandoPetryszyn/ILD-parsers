package org.uqbar.thin.parsers

import scala.util.parsing.combinator._

object XMLParser extends XMLParser
trait XMLParser extends RegexParsers {
  
  //No se como representar a las tags que no tienen cuerpo enteramente, asi que por ahora es simplemente un string vacio (en vez de otra clase)
  //Es facilmente cambiable
  trait Element
  //Representa algo que tiene hijos
  case class Node(id: String,attrb: List[Attribute],body: String,children: List[Element]) extends Element
  //Representa las hojas del arbol
  case class Leaf(id: String,attrb: List[Attribute],body: String) extends Element
  
  case class Attribute(id: String,value:String)
  
	def apply(input: String) = parseAll(???, input) match {
		case Success(result, _) => result
		case NoSuccess(msg, _) => throw ParseException(msg)
	}
  
  
}