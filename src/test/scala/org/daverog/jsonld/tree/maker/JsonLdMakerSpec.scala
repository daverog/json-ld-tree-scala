
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
    it("does not change an empty scaffold and model"){
      assertJsonLdMadeCorrectly("", "{\n  \n}", "{\n  \n}")
    }
    it("does not change an empty scaffold and model with data"){
      assertJsonLdMadeCorrectly("<urn:a> <urn:b> <urn:c> .", "{\n  \n}", "{\n  \n}")
    }
    it("does not change an scaffold with empty model"){
      assertJsonLdMadeCorrectly("", "{\n  \"a\":\"b\"\n}", "{\n  \"a\":\"b\"\n}")
    }
    it("ignores an absent optional field value from a result item"){
      assertJsonLdMadeCorrectly("", 
		"""
	    {
		  "@id": "@optional"
		}
	    """,
	    "{}")
    }
    it("extracts an optional URI from a result item"){
      assertJsonLdMadeCorrectly(
		"""
		@prefix result: <http://purl.org/ontology/rdf-result/> .
		result:this result:item <urn:a> .
		""", 
		"""
		{
		  "@id": "@optional"
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
		  "@id": "@required"
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
		    "@id": "@required"
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
		    "field": "@required"
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
		  "urn:b": "@required"
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
		  "urn:b": "@optional"
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
		  "urn:b": "@required"
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
		  "urn:b": "@required"
		  "@context": {
		    "urn:b": {
		      "@type": "@id"
            }
          }
		}
		""", "")}.getMessage must include("The field 'urn:b' contains a literal when a URI was expected")
    }
  }
  
  def assertJsonLdMadeCorrectly(turtleRdf: String, scaffold: String, expectedJson: String) = {
    val madeJson = prettifyJson(JsonLdMaker.populateScaffold(ModelUtils.createJenaModel(turtleRdf), scaffold))
    madeJson.must(equal(prettifyJson(expectedJson)))
  }
  
  def prettifyJson(json: String): String = {
    pretty(render(parse(json)))
  }
}