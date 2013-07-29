package org.daverog.jsonld.tree.generator

import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.RDFNode
import com.hp.hpl.jena.rdf.model.Resource
import com.hp.hpl.jena.rdf.model.Statement;

case class RdfTree(
    parent: Option[RdfTree], 
    value: Option[Any] = None,
    list: Boolean = false,
    children: Map[Predicate, Seq[RdfTree]] = Map(),
    expanded: Boolean = false) {
  
  def withChildren(children: Map[Predicate, Seq[RdfTree]]) = {
    this.copy(children = children, expanded = true)
  }
  
  def isLiteral: Boolean = {
    value match {
      case None => false
      case uri: Some[URI] => false
      case _ => true
    }
  }
  
  def expandedFully: Boolean = {
    isLiteral || (expanded && children.map({
      case (predicate, trees) => trees
    }).flatten.filter(tree => !tree.expandedFully).isEmpty)
  }
	
}

case class Predicate(uri: String, inverse: Boolean) {}