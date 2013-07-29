/**
 * Generates JSON-LD data from RDF in a predictable
 * tree structure based on metadata about the desired
 * 'roots' of the tree to extract from the graph
 */
package org.daverog.jsonld.tree.maker

import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.model.RDFNode
import scala.collection.JavaConversions
import net.liftweb.json._
import net.liftweb.json.Extraction._
import org.daverog.jsonld.tree.generator.ProcessedModel

object JsonLdMaker {
  
  val ResultOntologyPrefix: String = "http://purl.org/ontology/rdf-result/"
  val RdfPrefix: String = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val RdfType: String = RdfPrefix + "type"
  val OwlPrefix: String = "http://www.w3.org/2002/07/owl#"
  
  private def populateScaffold(model: ProcessedModel, context: JsonLdContext, nodeUri: Option[String], scaffold: JValue, parentUri: Option[String]): JValue = {
    scaffold.transform({
      case JField("@id", JString("@optional")) => nodeUri match {
        case None => JNothing
        case Some(uri) => JField("@id", JString(uri))
      }
      case JField("@id", JString("@required")) => nodeUri match {
        case None => parentUri match {
          case None => throw new IllegalArgumentException("The root node was not indicated by the result ontology")
    	  case Some(uri) => throw new IllegalArgumentException("@value required for @id within node: " + uri)
        }
        case Some(uri) => JField("@id", JString(uri))
      }
      case JField(fieldName, JString("@optional")) => getField(model, context, fieldName, nodeUri, parentUri, false)
      case JField(fieldName, JString("@required")) => getField(model, context, fieldName, nodeUri, parentUri, true)
    })
  }

  private def getField(model: ProcessedModel, context: JsonLdContext, fieldUri: String, nodeUri: Option[String], parentUri: Option[String], required: Boolean): JValue = {
    val predicateMap: Map[String, Seq[RDFNode]] = nodeUri match {
      case None => Map()
      case Some(nodeUri) => model.nonTypeObjectsBySubject.get(nodeUri).getOrElse(Map())
    }
    val fieldValues: List[RDFNode] = predicateMap.get(fieldUri).getOrElse(List()).toList
    fieldValues match {
      case Nil => required match {
        case true => invalid("The field '%s' is not present, but required".format(fieldUri), nodeUri, parentUri)
          
        case false => JNothing
      }
      case item :: Nil => (item.isResource, isPredicateRangeResource(context, fieldUri)) match {
        case (true,  true)  => JField(fieldUri, JString(item.asResource.getURI))
        case (true,  false) => invalid("A resource in the RDF was defined as a literal value in the @context", nodeUri, parentUri)
        case (false, true)  => invalid("A literal in the RDF was defined as a resource in the @context", nodeUri, parentUri)
        case (false, false) => JField(fieldUri, JString(item.asLiteral.getString))
      }
    }
  }
  
  def invalid(message: String, nodeUri: Option[String], parentUri: Option[String]) = {
    throw new IllegalArgumentException("%s within %s".format(message, describeNode(nodeUri), describeParent(parentUri)))
  }
  
  private def isPredicateRangeResource(context: JsonLdContext, fieldUri: String): Boolean = {
    //context.`@context`.get(fieldUri).map(_.)  //<<< working here!!!
    true
  }
  
  private def describeParent(parentUri: Option[String]): String = {
    parentUri match {
      case None => "the RDF-result-defined root node"
      case Some(parentUri) => "node <%s>".format(parentUri)
    }
  }

  private def describeNode(nodeUri: Option[String]): String = {
	nodeUri match {
	  case None => "no defined position"
	  case Some(nodeUri) => "node <%s>".format(nodeUri)
  }
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
    
    val context = scaffold.extract[JsonLdContext]
    
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
    
    pretty(render(populateScaffold(processedModel, context, items.headOption, scaffold, None)))
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


case class JsonLdContext(`@context`: Map[String, JsonLdContextReference])
case class JsonLdContextReference(`@id`: String, `@type`: String)

