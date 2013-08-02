/**
 * Generates JSON-LD data from RDF in a predictable
 * tree structure based on metadata about the desired
 * 'roots' of the tree to extract from the graph
 */
package org.daverog.jsonld.tree.generator

import scala.collection.JavaConversions
import scala.collection.immutable.TreeMap
import scala.xml.Node
import scala.xml.XML

import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.Statement

object RdfResult {
  
  val ResultOntologyPrefix: String = "http://purl.org/ontology/rdf-result/"
  val RdfPrefix: String = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val RdfType: String = RdfPrefix + "type"
  val OwlPrefix: String = "http://www.w3.org/2002/07/owl#"
  
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
  
  def generateTree(model: Model): RdfTree = {
	val categorisedStatements: Seq[(StatementType, Statement)] = JavaConversions
		.collectionAsScalaIterable(model.listStatements.toList).toList
		.map(statement => (categoriseStatement(statement), statement))
	
    val items = categorisedStatements.collect { case (Item, statement) => statement.getObject.asResource.getURI }
	val data = categorisedStatements.collect { case (Data, statement) => statement }
	
	if (data.isEmpty) return RdfTree(None, None)
	
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
    
    items.length match {
      case 0 => throw new IllegalArgumentException("result:this is not present as the subject of a statement, so an RDF tree cannot be generated")
      case 1 => buildTree(items.head, processedModel)
      case _ => throw new IllegalArgumentException("More than one result:this subject was found for a single item result")
    }
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
  
  def treeToSortedMap(tree: RdfTree): Any = {
    if (tree.children.isEmpty) return tree.value match {
      case Some(uri: URI) => uri.uri
      case Some(x) => x
      case None => new Object 
    }
    
    val children: TreeMap[String, Any] = TreeMap(tree.children.filter({
        case (predicate, childrenForPredicate) => !predicate.inverse
      }).map({
        case (predicate, childrenForPredicate) => (predicate.uri, collapse(childrenForPredicate.map(treeToSortedMap(_))))
      }).toArray:_*)
    val inverseChildren: TreeMap[String, Any] = TreeMap(tree.children.filter({
      	case (predicate, childrenForPredicate) => predicate.inverse
      }).map({
      	case (predicate, childrenForPredicate) => (predicate.uri, collapse(childrenForPredicate.map(treeToSortedMap(_))))
      }).toArray:_*)
      
    val withId = children.updated("@id", tree.value.get.asInstanceOf[URI].uri)
    
    inverseChildren.isEmpty match {
      case true => withId
      case false => withId.updated("@reverse", inverseChildren)
    }
  }

  def treeToXmlLd(tree: RdfTree): Node = {
    val xml = "<xml-ld><results>" + xmlTree(treeToSortedMap(tree)) + "</results><context></context></xml-ld>"
    println(xml)
    XML.loadString(xml)
  }
  
  def xmlTree(tree: Any): String = {
    tree match {
      case map: Map[String, Any] => {
        "<Thing%s>%s</Thing>".format(
          map.map{
            case (predicate, value: String) if (predicate.head == '@') => 
              	" " + predicate.replace("@", "") + "=\"" + value + "\""
            case _ => ""
          }.foldLeft("")((b,a) => b ++ a),
          map.map{
            case (predicate, value: String) if (predicate.head == '@') => ""
            case (predicate, child: String) =>
            	"<" + predicate + " id=\""+child+"\"/>" 
            case (predicate, child) if (predicate.head != '@') => 
              	"<" + predicate + ">" + xmlTree(child) + "</" + predicate + ">" 
            case _ => "help!"
          }.foldLeft("")((b,a) => b ++ a))
      }
      case text: String => text
      case _ => "help!"
    }
  }
  
  def collapse(thing: Any): Any = {
    thing match {
      case seq: Seq[Any] => seq.length match {
	    case 1 => seq.head
	    case _ => seq
      }
      case _ => thing
    }
  }
  
  def buildTree(rootUri: String, model: ProcessedModel) = {
    expandFully(RdfTree(None, Some(URI(rootUri))), model)
  }
  
  def expandFully(tree: RdfTree, model: ProcessedModel): RdfTree = {
    tree.expandedFully match {
      case true => tree
      case false => tree.expanded match {
        case true => {
          val expandedChildren = tree.children.map({
            case (predicate, children) => predicate -> children.map(child => child.expandedFully match {
              case true => child
              case false => expandFully(child, model)
            })
          })
          tree.withChildren(expandedChildren)
        }
        case false => tree.withChildren(getChildrenOfTreeBasedOnModel(tree, model))
      }      
    }
  }
  
  def getChildrenOfTreeBasedOnModel(tree: RdfTree, model: ProcessedModel): Map[Predicate, Seq[RdfTree]] = {
    val nodeUri = tree.value.get.asInstanceOf[URI].uri
    val objectsWhereNodeIsSubject= model.nonTypeObjectsBySubject.get(nodeUri).getOrElse(Seq())
    val subjectsWhereNodeIsObject = model.subjectsByObject.get(nodeUri).getOrElse(Seq())
    val types = model.types.get(nodeUri).getOrElse(Seq())
		
    val singleType = types.headOption
			
    val children = objectsWhereNodeIsSubject.map({
	  case (predicateUri, objectNodes) => {
	    Predicate(predicateUri, false) -> objectNodes.map(objectNode => RdfTree(Some(tree), Some(rdfNodeToValue(objectNode)), false))
	  }
	}).toMap

	val inverseChildren = subjectsWhereNodeIsObject.map({
	  case (predicateUri, subjects) => {
		Predicate(predicateUri, true) -> subjects.map(subject => RdfTree(Some(tree), Some(URI(subject)), false))
	  }
	}).toMap
    
	children ++ inverseChildren
  }
  
  def rdfNodeToValue(node: RDFNode): Any = {
    if (node.isResource()) URI(node.asResource.getURI)
    else if (node.isLiteral()) node.asLiteral.getValue
    else throw new IllegalArgumentException("error")
  }
  
}

case class ProcessedModel(
    types: Map[String, Seq[String]], 
    nonTypeObjectsBySubject: Map[String, Map[String, Seq[RDFNode]]], 
    subjectsByObject: Map[String, Map[String, Seq[String]]]) {}

case class URI(uri: String)