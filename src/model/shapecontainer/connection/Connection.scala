package model.shapecontainer.connection

import model.shapecontainer.ShapeContainerElement
import model.style.Style
import util.{Cache, PlacingSketch, CommonParserMethodes}
import model.CacheEvaluation._

/**
 * Created by julian on 20.10.15.
 * representation of a Connection
 * @param connection_type -> inner Connection.scala ConnectionStyle, can either be an object {FreeForm, Manhatten}
 * @param style is a model.style.Style instance
 * @param placing outstanding
 * TODO
 */
case class Connection(name:String,
                 connection_type:Option[ConnectionStyle] = None,
                 style:Option[Style] = None,
                 placing:List[Placing] = List[Placing]()) extends ShapeContainerElement

object Connection extends CommonParserMethodes{
  val validConnectionAttributes = List("connection-type", "layout", "placing")
  /**
   * parse method
   * */
  def apply(name:String,
            styleRef:Option[String],
            typ:Option[String],
            anonymousStyle:Option[String],
            placings:List[PlacingSketch],
            hierarchyContainer:Cache):Option[Connection] = {
    implicit val cache = hierarchyContainer
    /*mapping*/
    var style:Option[Style] = if(styleRef isDefined) styleRef.get else None
    val connection_type:Option[ConnectionStyle] = if(typ isDefined) Some(parse(connectionType, typ.get).get) else None
    if(anonymousStyle.isDefined) {
      style = Style.makeLove(cache, style, anonymousStyle)
    }
    val placingList = placings.map{Placing(_, style, cache.shapeHierarchy.root.data)}

    if(placingList isEmpty)
      None
    else {
      val newConnection = new Connection(name, connection_type, style, placingList)
      cache + newConnection
      Some(newConnection)
    }
  }

  def connectionType = "connection-type\\s*=".r ~> "(freeform|manhatten)".r ^^ {
    case "freeform" => FreeForm
    case "manhatten" => Manhatten
  }

}

abstract sealed class ConnectionStyle
  case object FreeForm extends ConnectionStyle
  case object Manhatten extends ConnectionStyle
