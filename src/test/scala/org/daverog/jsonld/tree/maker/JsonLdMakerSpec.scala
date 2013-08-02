
package org.daverog.jsonld.tree.maker

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.scalatest.BeforeAndAfter
import net.liftweb.json._
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonDSL._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.daverog.jena.utils.ModelUtils


@RunWith(classOf[JUnitRunner])
class JsonLdMakerSpec extends FunSpec with MustMatchers with BeforeAndAfter {
  
  describe("JsonLdMaker.populateScaffold") {
    it("extracts an optional URI from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
		""", 
		"""
		{
		  "@id": "$optional"
		}
		""",
		"""
		{
		  "@id":"urn:a"
		}
		""")
    }
	it("extracts a required URI from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
		""", 
		"""
		{
		  "@id": "$required"
		}
		""",
		"""
		{
		  "@id":"urn:a"
		}
    	""")
    }
	it("fails if a required URI is not provided by a result item"){
	  intercept[IllegalArgumentException] {
        assertJsonLdMadeCorrectly("", 
		  """
	  	  { 
		    "@id": "$required"
	  	  }
		  """, "")
      }.getMessage must include("The root node was not indicated by the result ontology")
	}
	it("fails if a required field value is not present in the result item"){
	  intercept[IllegalArgumentException] {
	    assertJsonLdMadeCorrectly(
	      """
		  @prefix result: <http://purl.org/ontology/rdf-result/> .
		  result:this result:item <urn:a> .
		  """, 
		  """
	      { 
		    "field": "$required"
		  }
		  """, "")
		}.getMessage must include("The field 'field' is not present")
	}
	it("extracts a required field value from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> "value" .
		""", 
		"""
		{
		  "urn:b": "$required"
		}
		""",
		"""
		{
		  "urn:b":"value"
		}
    	""")
    }
    it("extracts an optional field value from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> "value" .
		""", 
		"""
		{
		  "urn:b": "$optional"
		}
		""",
		"""
		{
		  "urn:b":"value"
		}
    	""")
    }
    it("extracts a required field URI value from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
		""", 
		"""
		{
		  "urn:b": "$required"
		  "@context": {
		    "urn:b": {
		      "@type": "@id"
            }
          }
		}
		""",
		"""
		{
		  "urn:b":"urn:c"
	      "@context": {
		    "urn:b": {
		      "@type": "@id"
            }
          }
		}
    	""")
    }
    it("fails if a URI is expected (because the predicate has @type = @id) but a literal is found"){
      intercept[IllegalArgumentException] { assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> "value" .
		""", 
		"""
		{
		  "urn:b": "$required",
		  "@context": {
		    "urn:b": {
		      "@type": "@id"
            }
          }
		}
		""", "")}.getMessage must include("A literal in the RDF was defined as a resource in the @context")
    }
    it("extracts an optional URI from a result item into an array"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
		""", 
		"""
		{
		  "urn:b": [ "$optional" ]
		}
		""",
		"""
		{
		  "urn:b": [ "urn:c" ]
		}
		""")
    }
    it("extracts optional URIs from a result item into an array"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
        <urn:a> <urn:b> <urn:d> .
		""", 
		"""
		{
		  "urn:b": [ "$optional" ]
		}
		""",
		"""
		{
		  "urn:b": [ "urn:c", "urn:d" ]
		}
		""")
    }
    it("extracts required URIs from a result item into an array"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
        <urn:a> <urn:b> <urn:d> .
		""", 
		"""
		{
		  "urn:b": []
		}
		""",
		"""
		{
		  "urn:b": [ "urn:c", "urn:d" ]
		}
		""")
    }
    it("extracts required URIs from a result item into an array defined by $array"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
        <urn:a> <urn:b> <urn:d> .
		""", 
		"""
		{
		  "urn:b": { "$array" : "true" }
		}
		""",
		"""
		{
		  "urn:b": [ "urn:c", "urn:d" ]
		}
		""")
    }
    it("fails if an $only field has more than one value"){
      intercept[IllegalArgumentException] { assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
        <urn:a> <urn:b> <urn:c> .
        <urn:a> <urn:b> <urn:d> .
		""", 
		"""
		{
		  "urn:b": "$only"
		}
		""","")
      }.getMessage must include("A field defined as 'only' cannot have 2 values")
    }
  }
  
  def assertJsonLdMadeCorrectly(turtleRdf: String, scaffold: String, expectedJson: String) = {
    val madeJson = prettifyJson(JsonLdMaker.populateScaffold(ModelUtils.createJenaModel(turtleRdf), scaffold))
    madeJson.must(equal(prettifyJson(expectedJson)))
  }
  
  def prettifyJson(json: String): String = {
    pretty(render(parse(json)))
  }
  
  def ideas = {
    """
    [] == { "$array" : "true" }
    [ "hello" ] == { 
      "$array" : "true" ,
      "$defaultContents" : [ "hello" ]
    }
    
	"$array" : "true",
	"$minSize" : 1,
	"$maxSize" : 2,
	"$truncate" : 3
    "$hideIfEmpty" : "true"
    "$defaultContents" : ["a", "b"]
    """
  }
}