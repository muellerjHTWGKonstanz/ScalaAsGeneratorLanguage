package model.diagram.edge

import model.diagram.action.LocalActionGroup
import model.diagram.node.ShapeProperty
import model.diagram.traits.{OnCreate, Container, Palette}
import model.style.{Style, HasStyle}

/**
 * Created by julian on 30.11.15.
 * representation of an Edge
 */
case class Edge(name:String,
                 connection:Connection,
                 from:String,
                 to:String,
                 actionGroup:Option[LocalActionGroup] = None,
                 override val style: Option[Style] = None,
                 override val palletteCompartment: String = "",
                 override val containmentReference: String = "",
                 override val askFor: String = ""
                 ) extends HasStyle with Palette with Container with OnCreate{
}

case class Connection(connection:model.shapecontainer.connection.Connection,
                       connectionProperties:List[ShapeProperty])
