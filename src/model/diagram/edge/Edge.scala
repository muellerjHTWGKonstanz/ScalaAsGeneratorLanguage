package model.diagram.edge

import model.HierarchyContainer
import model.diagram.action.{Action, Actions}
import model.diagram.methodes.{OnDelete, OnUpdate, OnCreate, Methodes}
import model.diagram.node.ShapeProperty
import model.diagram.traits.{Container, Palette}
import model.style.{Style, HasStyle}

/**
 * Created by julian on 30.11.15.
 * representation of an Edge
 */
case class Edge(name:String,
                ecoreElement:AnyRef,
                override val style: Option[Style] = None,
                            /*edge-Block*/
                connection:Connection,
                from_ID:String,
                to_ID:String,
                override val palette: String = "",
                override val container: String = "",
                override val onCreate:Option[OnCreate] = None,
                override val onUpdate:Option[OnUpdate] = None,
                override val onDelete:Option[OnDelete] = None,
                override val action:Option[Action]     = None,
                override val actionIncludes: List[String] = List(),
                hierarchyContainer: HierarchyContainer
                ) extends HasStyle with Palette with Container with Methodes with Actions{
  val from = hierarchyContainer.shapeHierarchy(from_ID)
  val to = hierarchyContainer.shapeHierarchy(to_ID)
}

class Connection(connectionIdentifier:String,
                 val connectionProperties:List[ShapeProperty],
                 hierarchyContainer: HierarchyContainer){
  val connection:model.shapecontainer.connection.Connection = hierarchyContainer.connections(connectionIdentifier)
}


