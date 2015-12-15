package model.diagram

import model.diagram.action.ActionGroup
import model.diagram.edge.Edge
import model.diagram.node.Node
import model.style.{Style, HasStyle}

/**
 * Created by julian on 24.11.15.
 * representation of a diagram
 */
case class Diagram (name:String,
                    globalActionGroups:Map[String, ActionGroup],
                    nodes:Map[String, Node],
                    edges:Map[String, Edge],
                    override val style: Option[Style],
                    ecoreElement:AnyRef) extends HasStyle{
}

object Diagram {
  def apply(name:String, globActGrps:Map[String, ActionGroup], nodes:Map[String, Node], edges:Map[String, Edge], style:Option[Style], modelType:String)
    = new Diagram(name, globActGrps, nodes, edges, style, modelType)
}
