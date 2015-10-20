package model.shapecontainer.connection

import model.shapecontainer.ShapeContainerElement
import model.style.Style

/**
 * Created by julian on 20.10.15.
 * representation of a Connection
 * @param connection_type -> inner Connection.scala ConnectionStyle, can either be an object {FreeForm, Manhatten}
 * @param layout is a model.style.Style instance
 * @param placing outstanding
 * TODO
 */
class Connection(val connection_type:Option[ConnectionStyle] = None,
                  layout:Option[Style] = None,
                  placing:List[PlacingDefinition] = List[PlacingDefinition]()) extends ShapeContainerElement{

}

abstract sealed class ConnectionStyle
  case object FreeForm extends ConnectionStyle
  case object Manhatten extends ConnectionStyle
