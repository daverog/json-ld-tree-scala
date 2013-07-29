
package org.daverog.jsonld.tree.generator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.BeforeAndAfter
import net.liftweb.json._
import net.liftweb.json.Extraction._
import scala.xml.Node
import scala.xml.PrettyPrinter
import org.daverog.jsonld.tree.generator.RdfResult
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RdfResultSpec extends FunSpec with MustMatchers with BeforeAndAfter {
  
  implicit val formats = net.liftweb.json.DefaultFormats
  
  describe("RdfResult.treeToSortedMap") {
    it("generates an empty JSON object for an empty model"){
      prettify("{}").must(equal(generateJsonLd("")))
    }
    
    "result:this is not present as the subject of a statement, so an RDF tree cannot be generated"
    it("must reject RDF that does not indicate the root of the tree") {
      intercept[IllegalArgumentException] {
        generateJsonLd("<urn:a> <urn:b> <urn:c> .")
      }.getMessage must include("result:this is not present as the subject of a statement, so an RDF tree cannot be generated")
  	}
    it("rejects a model with two items") {
      intercept[IllegalArgumentException] {
        generateJsonLd(
		  """
		    @prefix result: <http://www.bbc.co.uk/ontologies/result/> .
		    result:this result:item <urn:a> .
		    result:this result:item <urn:b> .
		    <urn:a> <urn:b> <urn:c> .
		  """
        )
      }.getMessage must include("More than one result:this subject was found for a single item result")
  	}
    it("generates basic JSON") {
      prettify(
	  """
	    {
		  "@id": "urn:a",
		  "urn:b": "urn:c"
	    }
	  """).must(equal(generateJsonLd(
	  """
	    @prefix result: <http://www.bbc.co.uk/ontologies/result/> .
	    result:this result:item <urn:a> .
	    <urn:a> <urn:b> <urn:c> .
	  """
	  )))
  	}
    it("applies a reverse object to inverse properties") {
      prettify(
      """
		{
		"@id": "urn:c",
		"@reverse" : {
            "urn:b": "urn:a"
          }
		}
	  """).must(equal(generateJsonLd(
	  """
		@prefix result: <http://www.bbc.co.uk/ontologies/result/> .
		result:this result:item <urn:c> .
		<urn:a> <urn:b> <urn:c> .
	  """
	  )))
    }
  }

  describe("treeToXmlLd") {
    it("must generate basic XML") {
	  prettifyXml(
		<xml-ld>
          <results>
            <Thing id="urn:a">
    		  <urn:b id="urn:c"/>
            </Thing>
          </results>
          <context></context>
        </xml-ld>).must(equal(generateXmlLd(
	  """
		@prefix result: <http://www.bbc.co.uk/ontologies/result/> .
		result:this result:item <urn:a> .
		<urn:a> <urn:b> <urn:c> .
	  """
	  )))
    }
	it("must handle escaping in XML") {
	  prettifyXml(
		<xml-ld>
          <results>
            <Thing id="urn:a">
    		  <uri:b>'string &amp; - string'</uri:b>
            </Thing>
          </results>
          <context></context>
        </xml-ld>).must(equal(generateXmlLd(
	  """
		@prefix result: <http://www.bbc.co.uk/ontologies/result/> .
		result:this result:item <urn:a> .
		<urn:a> <urn:b> 'string & - string' .
	  """
	  )))
    }
  }
  
  def generateJsonLd(rdf: String): String = {
    jsonMapToPrettyJson(RdfResult.treeToSortedMap(RdfResult.generateTree(ModelUtils.createJenaModel(rdf))))
  }

  def generateXmlLd(rdf: String): String = {
	prettifyXml(RdfResult.treeToXmlLd(RdfResult.generateTree(ModelUtils.createJenaModel(rdf))))
  }

  def jsonMapToPrettyJson(jsonMap: Any): String = {
	pretty(render(decompose(jsonMap)))
  }
  
  def prettify(json: String) = {
    pretty(render(parse(json)))
  }
  
  def prettifyXml(node: Node): String = {
    val builder = new StringBuilder
    new PrettyPrinter(80, 2).format(node, builder)
    builder.toString
  }
  
}