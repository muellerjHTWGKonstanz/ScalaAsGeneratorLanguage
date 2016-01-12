package model.diagram

import model.diagram.action.ActionGroup
import model.diagram.edge.Edge
import model.diagram.node.Node
import model.style.{Style, HasStyle}
import util.Cache

/**
 * Created by julian on 24.11.15.
 * representation of a diagram
 */
class Diagram (val name:String,
               val globalActionGroups:Map[String, ActionGroup],
               val nodes:List[Node],
               val edges:List[Edge],
               override val style: Option[Style],
               val ecoreElement:AnyRef) extends HasStyle

object Diagram {
  def apply(name:String, globActGrps:Map[String, ActionGroup], nodes:List[Node], edges:List[Edge], style:Option[Style], modelType:String, cache:Cache) = {
    /*since the style cant be crossbreeded while parsing, since all the style instances might not even be existent at that time -> now crossbreed the styles
    * to ensure nodes and edges inherit the according style properties from their parent diagram*/
    nodes foreach(n => n.style = Style.makeLove(cache, style, n.style))
    edges foreach(e => e.style = Style.makeLove(cache, style, e.style))
    new Diagram(name, globActGrps, nodes, edges, style, modelType)
  }
}
