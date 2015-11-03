package model.shapecontainer.shape

import model.Diagram
import model.shapecontainer.ShapeContainerElement
import model.shapecontainer.shape.anchor.Anchor
import model.shapecontainer.shape.anchor.Anchor.AnchorType
import model.shapecontainer.shape.geometrics.{Description, GeometricModel}
import model.style.Style
import util.CommonParserMethodes

/**
 * Created by julian on 29.09.15.
 * The actual representation of a shape class,
 * will hold all the relevant attributes
 *
 * @param name = id
 * @param style = model.style.Style
 * @param shape is a list of model.shapecontainer.shape.geometrics.GeometricForm 's that can be nested inside the shape
 * TODO
 */
case class Shape( name:String = "no name",
                  style:Option[Style]                   = None,
                  size_width_min:Option[Int]            = None, /*from ShapeLayout*/
                  size_height_min:Option[Int]           = None, /*from ShapeLayout*/
                  size_width_max:Option[Int]            = None, /*from ShapeLayout*/
                  size_height_max:Option[Int]           = None, /*from ShapeLayout*/
                  stretching_horizontal:Option[Boolean] = None, /*from ShapeLayout*/
                  stretching_vertical:Option[Boolean]   = None, /*from ShapeLayout*/
                  proportional:Option[Boolean]          = None, /*from ShapeLayout*/

                  shape:Option[List[GeometricModel]]    = None,

                  description:Option[Description]            = None,

                  anchor:Option[AnchorType]             = None,

                  extendedShape:List[Shape] = List()) extends ShapeContainerElement{
  val key = hashCode
  def apply() = shape
}

object ShapeParser extends CommonParserMethodes{
  def apply(name:String, style:Option[String], attributes:List[(String, String)], geos:List[GeometricModel], diagram:Diagram) =
    parse(name, style, attributes, geos, diagram)

  def parse(name:String, style:Option[String], attributes:List[(String, String)], geos:List[GeometricModel], diagram:Diagram):Shape = {
    var styl:Option[Style] = None
    if(style isDefined){
      val ret = diagram.styleHierarchy.nodeView.get(style.get)
      if(ret isDefined)
        styl = Some(ret.get.data)
    }
    var size_width_min:Option[Int]    = None
    var size_width_max:Option[Int]    = None
    var size_height_min:Option[Int]   = None
    var size_height_max:Option[Int]   = None
    var stretching_horizontal:Option[Boolean] = None
    var stretching_vertical:Option[Boolean]   = None
    var prop:Option[Boolean]          = None
    var description:Option[Description]            = None
    var anchor:Option[AnchorType]             = None
    val chilldrenGeometricModels = if(geos nonEmpty) Some(geos) else None

    attributes.foreach{
      case x if x._1.matches("size[-_]min") =>
        val opt = parse(width_height, x._2).get
        if(opt.isDefined){
          size_width_min = Some(opt.get._1)
          size_height_min = Some(opt.get._2)
        }
      case x if x._1.matches("size[-_]max") =>
        val opt = parse(width_height, x._2).get
        if(opt.isDefined){
          size_width_max = Some(opt.get._1)
          size_height_max = Some(opt.get._2)
        }
      case x if x._1 =="stretching" =>
        val opt = parse(stretching, x._2).get
        if(opt.isDefined){
          stretching_horizontal = Some(opt.get._1)
          stretching_vertical = Some(opt.get._2)
        }
      case x if x._1 == "proportional" =>
        prop = parse(proportional, x._2).get
      case x if x._1 =="anchor" => anchor = {
        val anch = Anchor.parse(Anchor.anchor, x._2)
        if(anch isEmpty)
          None
        else
          Some(anch.get)
      }
      case x if x._1.matches("description.*") => description = Description.parse(x, diagram)
      case _ =>
    }

    /*create the actual shape instance*/
    new Shape(name, styl, size_width_min, size_width_max, size_height_min, size_height_max,
      stretching_horizontal, stretching_vertical, prop, chilldrenGeometricModels, description, anchor)

  }

  def proportional:Parser[Option[Boolean]] = "=?".r ~> argument ^^ {
    case prop => Some(matchBoolean(prop))
    case _ => None
  }
  def stretching:Parser[Option[(Boolean, Boolean)]] = "\\(\\s*(horizontal=)?".r ~> argument ~ (",\\s*(vertical=)?".r ~> argument) <~ ")" ^^ {
    case hor ~ ver => Some(matchBoolean(hor), matchBoolean(ver))
    case _ => None
  }
  def width_height:Parser[Option[(Int, Int)]] = "\\(\\s*(width=)?".r ~> argument ~ (",\\s*(height=)?".r ~> argument) <~ ")" ^^ {
    case width ~ height => Some((width.toInt, height.toInt))
    case _ => None }
}
