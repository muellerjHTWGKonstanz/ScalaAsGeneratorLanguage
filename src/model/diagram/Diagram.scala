package model.diagram

import model.diagram.action.{ActionGroup, ActionGroup$}
import model.diagram.edge.Edge
import model.diagram.node.Node
import model.style.{Style, HasStyle}

/**
 * Created by julian on 24.11.15.
 * representation of a diagram
 */
class Diagram ( val globalActionGroups:Map[String, ActionGroup],
                val nodes:List[Node],
                val edges:List[Edge],
                override val style: Option[Style],
                val ecoreElement:Object) extends HasStyle{
}

object Diagram {
  def apply(globActGrps:Map[String, ActionGroup], nodesAndEdges:List[Any], style:Option[Style], modelType:String)
    = new Diagram(globActGrps, nodesAndEdges.filter(_.isInstanceOf).asInstanceOf, nodesAndEdges.filter(_.asInstanceOf).asInstanceOf, style, modelType)
}
