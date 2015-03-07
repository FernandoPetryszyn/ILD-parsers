package org.uqbar.thin.parsers

class XMLParserTest extends ParserTest with XMLParser {
  "XML Parser" - {

    "should parse" - {

      "single tag with body and no attributes" in {
        "<ble>hola</ble>" should beParsedTo(Leaf("ble",Nil,"hola"))(leaf)
      }
      
      "single tag no body no attributes" in {
        "<ble></ble>" should beParsedTo(Leaf("ble",Nil,""))(leaf)
      }
      
      "single tag no body with attributes" in {
        "<person gender=female></person>" should beParsedTo(Leaf("person",List(Attribute("gender","female")),""))(leaf)
      }
      
      "simple xmlFile" in {
        """<start>                                                        
            <ble>hola</ble>
           </start>""" should beParsedTo(Node("start",Nil,List(Leaf("ble",Nil,"hola"))))(node)
      }
      
      "moderate xmlFile" in {
        """<start test=on>
                <tag> chau </tag>
                <ble> hola </ble>
                <tagConAtributo atributo=si> smile </tagConAtributo>
                <nodoExtra> 
                    <ble> vacio </ble>
                </nodoExtra>
           </start>""" should beParsedTo(Node("start",List(Attribute("test","on")),List(Leaf("tag",Nil,"chau"),Leaf("ble",Nil,"hola"),Leaf("tagConAtributo",List(Attribute("atributo","si")),"smile"),Node("nodoExtra",Nil,List(Leaf("ble",Nil,"vacio"))))))(node)
      }
      
 /*     "complex xmlFile" in {
        """<start test=on>
                <tag> chau </tag>
                <ble> hola </ble>
                <tagConAtributo atributo=si> smile </tagConAtributo>
                <nodoExtra> 
                    <ble> vacio </ble>
                </nodoExtra>
           </start>
          <unaHojaColgada></unaHojaColgada>
          """ should beParsedTo(List(Node("start",List(Attribute("test","on")),List(Leaf("tag",Nil,"chau"),Leaf("ble",Nil,"hola"),Leaf("tagConAtributo",List(Attribute("atributo","si")),"smile"),Node("nodoExtra",Nil,List(Leaf("ble",Nil,"vacio"))))),Leaf("unaHojaColgada",Nil,"")))(xml.*)
         
      } */

    }
    
    "should not parse"  - {
      "fruta" in {
        "@@@@" should notBeParsed(xml)
      }
      
      
  /* Si pones mas lindo el XMLParser este no va a tirar error :P  
   *  "typo" in {
        "<tag></notag>" should notBeParsed(xml)
        }
        */
    }
  }
}