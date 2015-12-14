package model.diagram

import model.diagram.action.{ActionGroup, ActionGroup$}
import model.diagram.edge.Edge
import model.diagram.node.Node
import model.style.{Style, HasStyle}

/**
 * Created by julian on 24.11.15.
 * representation of a diagram
 */
class Diagram ( val name:String,
                val globalActionGroups:Map[String, ActionGroup],
                val nodes:Map[String, Node],
                val edges:Map[String, Edge],
                override val style: Option[Style],
                val ecoreElement:Object) extends HasStyle{
}

object Diagram {
  def apply(name:String, globActGrps:Map[String, ActionGroup], nodes:Map[String, Node], edges:Map[String, Edge], style:Option[Style], modelType:String)
    = new Diagram(name, globActGrps, nodes, edges, style, modelType)
}
