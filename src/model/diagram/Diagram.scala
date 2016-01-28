package model.diagram

import model.diagram.action.ActionGroup
import model.diagram.edge.Edge
import model.diagram.node.Node
import model.style.{Style, HasStyle}
import parser.Cache

/**
 * Created by julian on 24.11.15.
 * representation of a diagram
 */
sealed class Diagram private (val name:String,
               val globalActionGroups:Map[String, ActionGroup],
               val nodes:List[Node],
               val edges:List[Edge],
               override val style: Option[Style],
               val metamodelElement:AnyRef) extends HasStyle

object Diagram {
  def apply(name:String, globActGrps:Map[String, ActionGroup], nodes:List[Node], edges:List[Edge], style:Option[Style], modelType:String, cache:Cache) = {
    new Diagram(name, globActGrps, nodes, edges, style, modelType)
  }
}
