package org.daverog.jena.utils

import java.io.StringReader
import java.io.StringWriter
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import com.hp.hpl.jena.datatypes.DatatypeFormatException
import com.hp.hpl.jena.n3.turtle.TurtleParseException
import com.hp.hpl.jena.rdf.model.Literal
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.Statement
import scala.util.Random


object ModelUtils {

  val DefaultBaseUri = "http://www.bbc.co.uk/ontologies/unknown/"
 
  def createDefaultModel: Model = {
    ModelFactory.createDefaultModel
  }

  def createJenaModel(rdf: String): Model = {
    try {
      createJenaModel(rdf, "TTL") 
    } catch {
      case e: TurtleParseException => throw new IllegalArgumentException("The data provided was not valid Turtle: " + e.getMessage + "\n" + rdf)
    }
  }

  def createJenaModel(rdf: String, lang: String): Model = {
    val model: Model = ModelFactory.createDefaultModel
    model.read(new StringReader(rdf), DefaultBaseUri, lang)
    validateRdf(model)
    model
  }
  
  def generateRandomModel: Model = {
    val model = createDefaultModel
    1 to 10 foreach { _ => model.add(createJenaModel("<urn:a%s> <urn:b%s> <urn:c%s> ."
        .format(randomInt, randomInt, randomInt))) }
    model
  }
  
  private def randomInt: Int = {
    Random.nextInt(100)
  }

  /* Validation */

  def validateRdf(model: Model) {
    val stringWriter = new StringWriter()
    model.write(stringWriter, "TURTLE")
    model.listStatements.foreach(statement => {
      if (statement.getObject.isResource) {
        validateResource(statement.getObject.asResource, statement)
      } else if (statement.getObject.isLiteral) {
        validateLiteral(statement.getObject.asLiteral, statement)
      }
      if (statement.getSubject.isResource) {
        validateResource(statement.getSubject.asResource, statement, true)
      }
    })
  }

  def validateResource(resource: Resource, statement: Statement, resourceIsSubject: Boolean = false) {
    val uri = resource.asResource.getURI
    val namespace = resource.asResource.getNameSpace
    if (resource.isAnon == false) {
      val friendlyResource = resource.asResource.getURI.replace(DefaultBaseUri, "")
      val item = if (resourceIsSubject == true) "subject" else "object"
      if (namespace == DefaultBaseUri || !uri.contains(":")) {
        throw new IllegalArgumentException("The " + item + " <" + friendlyResource + "> " +
          "for predicate <" + statement.getPredicate + "> " +
          "was a relative URI, which is not allowed - it must have a prefix " +
          "(eg. <prefix:" + resource + ">)")
      }
    }
  }

  def validateLiteral(literal: Literal, statement: Statement) {
    try {
      literal.asLiteral.getValue
    } catch {
      case e: DatatypeFormatException => {
        val literalString = literal.asLiteral.toString
        throw new IllegalArgumentException("The literal \"" + literal.asLiteral.getString +
          "\" for predicate <" + statement.getPredicate + "> was not a valid " +
          "<" + literal.getDatatypeURI + ">")
      }
    }
  }

  def createNonEmptyModel(rdf: String): Model = {
    val model = createJenaModel(rdf) 
    if (model.isEmpty)
      throw new IllegalArgumentException("The data provided contained no statements.")
    model
  }

  private def getNodeAsString(node: RDFNode): String = {
    if (node.isLiteral())
      return (
        node.asLiteral.getValue.toString + ">" +
        node.asLiteral.getDatatypeURI + ">" +
        node.asLiteral.getLanguage.toLowerCase)
    if (node.isAnon())
      return "<urn:blank-node>"
    node.asResource().getURI()
  }


  def canonicalString(model: Model): String = {
    (model.listStatements.toList
      map (x => getNodeAsString(x.getSubject) + getNodeAsString(x.getPredicate) + getNodeAsString(x.getObject()))
      sortWith (_ < _) mkString)
  }
  
  def sparqlUpdateData(model: Model): String = {
    (model.listStatements.toList
      map (x => "    <%s> <%s> %s .\n".format(x.getSubject.asResource.getURI, x.getPredicate.asResource.getURI, getNodeAsTurtle(x.getObject)))
      sortWith (_ < _) mkString)
  }
  
  def getNodeAsTurtle(node: RDFNode): String = {
    if (node.isResource()) return "<%s>".format(node.asResource.getURI)
    if (node.isLiteral()) return "\"%s\"%s%s".format(
        node.asLiteral.getLexicalForm, 
        getLiteralTypeAsTurtle(node.asLiteral), 
        getStringLanguageAsTurtle(node.asLiteral))
    throw new IllegalArgumentException("RDFNode of this type not supported")
  }
  
  def getLiteralTypeAsTurtle(literal: Literal): String = {
    if (literal.getDatatypeURI == null) return ""
    "^^<%s>".format(literal.getDatatypeURI)
  }
  
  def getStringLanguageAsTurtle(literal: Literal): String = {
    if (literal.getLanguage == null || literal.getLanguage.isEmpty) return ""
	"@%s".format(literal.getLanguage)
  }

  def modelToRdf(model: Model): String = {
    validateRdf(model)
    val stringWriter = new StringWriter()
    model.write(stringWriter, "TURTLE")
    stringWriter.toString()
  }

}

case class Urn(content: String) {
  if (!content.matches("""^urn:([\w-]+:)+[\w-]+$"""))
    throw new IllegalArgumentException("Not a valid URN: " + content)
  
  override def toString = content
}