/**
 * Generates JSON-LD data from RDF in a predictable
 * tree structure based on metadata about the desired
 * 'roots' of the tree to extract from the graph
 */
package org.daverog.jsonld.tree.maker

import scala.collection.JavaConversions

import org.daverog.jsonld.tree.generator.ProcessedModel

import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.Statement

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JArray
import net.liftweb.json.JField
import net.liftweb.json.JNothing
import net.liftweb.json.JObject
import net.liftweb.json.JString
import net.liftweb.json.JValue
import net.liftweb.json.parse
import net.liftweb.json.pretty
import net.liftweb.json.render

object JsonLdMaker {
  
  val ResultOntologyPrefix: String = "http://purl.org/ontology/rdf-result/"
  val RdfPrefix: String = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val RdfType: String = RdfPrefix + "type"
  val OwlPrefix: String = "http://www.w3.org/2002/07/owl#"
  
  private def populateScaffold(model: ProcessedModel, context: JsonLdContext, nodeUri: String, scaffold: JValue, parentUri: Option[String]): JValue = {
    scaffold.transform({
      case JField(fieldUri, JString(fieldValue)) => {
        getFieldValue(model, context, fieldUri, fieldValue, nodeUri, parentUri) match {
	      case JNothing => JNothing
	      case JString(value) => JField(fieldUri, JString(value))
	      case JArray(list) => JField(fieldUri, JArray(list))
	    }
      }
      case JField(fieldUri, JArray(list)) => JField(fieldUri, getArray(model, context, fieldUri, list, nodeUri, parentUri))
      case JField(fieldUri, JObject(obj)) => JField(fieldUri, getObject(model, context, fieldUri, obj, nodeUri, parentUri))
    })
  }

  private def getObject(model: ProcessedModel, context: JsonLdContext, fieldUri: String, list: List[JField], nodeUri: String, parentUri: Option[String]): JValue = {
    val isArray =  list.contains(JField("$array", JString("true")))
    
    isArray match {
      case true  => getArray(model, context, fieldUri, List(), nodeUri, parentUri)
      case false => JObject(list)
    }
  }
  
  private def getArray(model: ProcessedModel, context: JsonLdContext, fieldUri: String, defaultContents: List[JValue], nodeUri: String, parentUri: Option[String]): JValue = {
    val predicateMap: Map[String, Seq[RDFNode]] = model.nonTypeObjectsBySubject.get(nodeUri).getOrElse(Map())
    val fieldValues: List[RDFNode] = predicateMap.get(fieldUri).getOrElse(List()).toList.sortBy(rdfNodeToSortableString(_))
    
    val fieldValuesContainsOnlyResources = fieldValues.foldLeft(true)((onlyResources, fieldValue) => fieldValue.isResource && onlyResources)
    
    (fieldValuesContainsOnlyResources, isPredicateRangeResource(context, fieldUri)) match {
      case (true,  Some(false)) => invalid("A resource in the RDF was defined as a literal value in the @context", nodeUri, parentUri)
      case (false, Some(true))  => invalid("A literal in the RDF was defined as a resource in the @context", nodeUri, parentUri)
      case (true,  _)           => JArray(fieldValues.map(fieldValue => JString(fieldValue.asResource.getURI)))
      case (false, _)           => JArray(fieldValues.map(fieldValue => JString(fieldValue.asLiteral.getString)))
    }
  }
  
  private def rdfNodeToSortableString(node: RDFNode): String = {
    node.isResource match {
      case true  => node.asResource.getURI
      case false => node.asLiteral.getString
    }
  }
  
  private def getFieldValue(model: ProcessedModel, context: JsonLdContext, fieldUri: String, fieldValue: String, nodeUri: String, parentUri: Option[String]): JValue = {
    val (required, only) = fieldValue match {
      case "$required" => (true,  false)
      case "$optional" => (false, false)
      case "$only"     => (false, true)
      case _ => return JString(fieldValue)
    } 

    if (fieldUri == "@id") return JString(nodeUri)
     
    val predicateMap: Map[String, Seq[RDFNode]] = model.nonTypeObjectsBySubject.get(nodeUri).getOrElse(Map())
    val fieldValues: List[RDFNode] = predicateMap.get(fieldUri).getOrElse(List()).toList
    
    fieldValues match {
      case Nil => required match {
        case true => invalid("The field '%s' is not present, but required".format(fieldUri), nodeUri, parentUri)
        case false => JNothing
      }
      case item :: Nil => getJStringFromRDFNode(item, isPredicateRangeResource(context, fieldUri), nodeUri, parentUri)
      case item :: items => {
        only match {
          case true => throw new IllegalArgumentException("A field defined as 'only' cannot have %s values".format(fieldValues.size))
          case false => getJStringFromRDFNode(item, isPredicateRangeResource(context, fieldUri), nodeUri, parentUri)
        }
      } 
    }
  }
  
  private def getJStringFromRDFNode(item: RDFNode, contextDeclaresItemAsResource: Option[Boolean], nodeUri: String, parentUri: Option[String]) = {
    (item.isResource, contextDeclaresItemAsResource) match {
        case (true,  Some(false)) => invalid("A resource in the RDF was defined as a literal value in the @context", nodeUri, parentUri)
        case (false, Some(true))  => invalid("A literal in the RDF was defined as a resource in the @context", nodeUri, parentUri)
        case (true,  _)           => JString(item.asResource.getURI)
        case (false, _)           => JString(item.asLiteral.getString)
      }
  }
  
  private def invalid(message: String, nodeUri: String, parentUri: Option[String]) = {
    throw new IllegalArgumentException("%s, within %s".format(message, describeNode(nodeUri), describeParent(parentUri)))
  }
  
  private def isPredicateRangeResource(context: JsonLdContext, fieldUri: String): Option[Boolean] = {
    context.getField(fieldUri) match {
      case None => None
      case Some(field) => field.`@type` match {
        case None => None
        case Some(t) => Some(t == "@id")
      }
    }
  }
  
  private def describeParent(parentUri: Option[String]): String = {
    parentUri match {
      case None => "the RDF-result-defined root node"
      case Some(parentUri) => "node <%s>".format(parentUri)
    }
  }

  private def describeNode(nodeUri: String): String = {
	"node <%s>".format(nodeUri)
  }
  
  object StatementType extends Enumeration {
    type StatementType = Value
    val Item, ListItem, ThisNext, Next, OrderByPredicate, Data = Value
  }
  import StatementType._

  object NodeType extends Enumeration {
	type NodeType = Value
	val ResultThis, ResultItem, ResultListItem, ResultNext, ResultOrderByPredicate, BlankNode, ResourceNode, LiteralNode = Value
  }
  import NodeType._
  
  def populateScaffold(model: Model, jsonScaffold: String): String = {
    implicit val formats = DefaultFormats
    val scaffold = parse(jsonScaffold)
    
    val context = scaffold match {
      case JNothing => JsonLdContext(None)
      case _ => scaffold.extract[JsonLdContext]
    }
    
	val categorisedStatements: Seq[(StatementType, Statement)] = JavaConversions
		.collectionAsScalaIterable(model.listStatements.toList).toList
		.map(statement => (categoriseStatement(statement), statement))
	
    val items = categorisedStatements.collect { case (Item, statement) => statement.getObject.asResource.getURI }
	val data = categorisedStatements.collect { case (Data, statement) => statement }
	
	val objectsBySubject: Map[String, Map[String, Seq[RDFNode]]] = data.groupBy(_.getSubject.getURI).map({
	  case (subjectUri, statements) => (subjectUri -> statements.groupBy(_.getPredicate.getURI).map({
	    case (predicateUri, nodes) => (predicateUri -> statements.map(_.getObject))
	  }))})
	  
    val subjectsByObject: Map[String, Map[String, Seq[String]]] = data.filter(_.getObject.isResource).groupBy(_.getObject.asResource.getURI).map({
	  case (objectUri, statements) => (objectUri -> statements.groupBy(_.getPredicate.getURI).map({
	    case (predicateUri, nodes) => (predicateUri -> statements.map(_.getSubject.getURI))
	  }))})
		
	val types = objectsBySubject.map({
	  case (subjectUri, predicateMap) => (subjectUri -> predicateMap.getOrElse(RdfType, Seq()).map(_.asResource.getURI))
	}).filter({
	  case (subjectUri, types) => !types.isEmpty
	})
	
	val nonTypeObjectsBySubject = objectsBySubject.map({
	  case (subjectUri, predicateMap) => (subjectUri -> predicateMap.filter({
	    case (predicateUri, nodes) => !predicateUri.equals(RdfType)
	  }))
	})
	
	val processedModel = ProcessedModel(types, nonTypeObjectsBySubject, subjectsByObject)
	
	if (items.isEmpty) throw new IllegalArgumentException("The root node was not indicated by the result ontology")
    
    pretty(render(populateScaffold(processedModel, context, items.head, scaffold, None)))
  }
    
  def categoriseStatement(statement: Statement): StatementType = {
    val categorised = (
        categoriseNode(statement.getSubject), 
        categoriseNode(statement.getPredicate), 
        categoriseNode(statement.getObject))

    categorised match {
      case (ResultThis, ResultItem, ResourceNode) => Item
      case (ResourceNode, _, _) => Data
      case (_, _, _) => throw new IllegalArgumentException("Unknown statement type, potential erroneous use of the result ontology")
    }    
  }
  
  def categoriseNode(node: RDFNode): NodeType = {
    if (node.isAnon) return BlankNode
    if (node.isLiteral) return LiteralNode
    node.asResource.getURI match {
      case n if n.equals(ResultOntologyPrefix + "this") => ResultThis
      case n if n.equals(ResultOntologyPrefix + "item") => ResultItem
      case n if n.equals(ResultOntologyPrefix + "next") => ResultNext
      case n if n.equals(ResultOntologyPrefix + "listItem") => ResultListItem
      case n if n.startsWith(ResultOntologyPrefix) => 
        throw new IllegalArgumentException("Result ontology URI <%s> is unknown".format(n))
      case _ => ResourceNode
    }
  }
  
}


case class JsonLdContext(`@context`: Option[Map[String, JsonLdContextReference]]) {
  def getField(fieldUri: String): Option[JsonLdContextReference] = {
    `@context` match {
      case None => None
      case _ => `@context`.get.get(fieldUri)
    }
  }  
}

case class JsonLdContextReference(`@id`: Option[String], `@type`: Option[String])