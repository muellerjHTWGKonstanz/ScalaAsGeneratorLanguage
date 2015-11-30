package model.shapecontainer.shape

import model.{ClassHierarchy, HierarchyContainer}
import model.shapecontainer.ShapeContainerElement
import model.shapecontainer.shape.anchor.Anchor
import model.shapecontainer.shape.anchor.Anchor.AnchorType
import model.shapecontainer.shape.geometrics.{Description, GeometricModel}
import model.style.{StyleParser, Style}
import util.{GeoModel, CommonParserMethodes}

/**
 * Created by julian on 29.09.15.
 * The actual representation of a shape class,
 * will hold all the relevant attributes
 *
 * @param name = id
 * @param style = model.style.Style
 * @param shapes is a list of model.shapecontainer.shape.geometrics.GeometricForm 's that can be nested inside the shape
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

                  shapes:Option[List[GeometricModel]]   = None,
                  description:Option[Description]       = None,
                  anchor:Option[AnchorType]             = None,
                  extendedShape:List[Shape] = List()) extends ShapeContainerElement{
  val key = hashCode
  def apply() = shapes
}

object ShapeParser extends CommonParserMethodes{
  val validShapeVariables = List("size-min", "size-max", "stretching", "proportional", "anchor", "description(\\s*style\\s*[a-zA-ZüäöÜÄÖ]+([-_][a-zA-ZüäöÜÄÖ])*)?\\s*")

  def apply(name:String, parents:Option[List[String]], style:Option[String], attributes:List[(String, String)], geos:List[GeoModel], description:Option[(String, String)], anchor:Option[String], hierarchyContainer:HierarchyContainer) =
    parse(name, parents, style, attributes, geos, description, anchor, hierarchyContainer)

  def parse(name:String,
            parentShapes:Option[List[String]],
            styleArgument:Option[String],
            attributes:List[(String, String)],
            geos:List[GeoModel],
            desc:Option[(String, String)],
            anch:Option[String],
            hierarchyContainer:HierarchyContainer):Shape = {

    val parents = if(parentShapes isDefined) parentShapes.get else List()
    var extendedStyle:List[Shape] = List[Shape]()
    if(parents.nonEmpty)
      parents.foreach{parent => {
        val parentName = parent.trim //trim just to make sure, could probably be removed
        if(hierarchyContainer.shapeHierarchy.contains(parentName))
          extendedStyle = hierarchyContainer.shapeHierarchy(parentName).data :: extendedStyle
      }
      }/*TODO if class was not found, to be inherited tell Logger*/

    /*mapping*/
    /** relevant is a help-methode, which shortens the actual call to mostRelevant of ClassHierarchy by ensuring the collection-parameter
      * relevant speaks for the hierarchical context -> "A extends B, C" -> C is most relevant */
    def relevant[T](f: Shape => Option[T]) = ClassHierarchy.mostRelevant(extendedStyle) {f}

    var style:Option[Style]                   = relevant {_.style}
    var size_width_min:Option[Int]            = relevant {_.size_width_min}
    var size_width_max:Option[Int]            = relevant {_.size_width_max}
    var size_height_min:Option[Int]           = relevant {_.size_height_min}
    var size_height_max:Option[Int]           = relevant {_.size_height_max}
    var stretching_horizontal:Option[Boolean] = relevant {_.stretching_horizontal}
    var stretching_vertical:Option[Boolean]   = relevant {_.stretching_vertical}
    var prop:Option[Boolean]                  = relevant {_.proportional}
    var description:Option[Description]       = relevant {_.description}
    var anchor:Option[AnchorType]             = relevant {_.anchor}
    val shapes:Option[List[GeometricModel]]   = relevant {_.shapes}

    /*initialize the mapping-variables with the actual parameter-values, if they exist*/
    if(styleArgument isDefined){
      val newStyle: Option[Style] = hierarchyContainer.styleHierarchy.get(styleArgument.get)
      if(newStyle isDefined) {
        if(style isDefined){
          style = StyleParser.makeLove(hierarchyContainer, style, newStyle)
        }else
          style = newStyle
      }
    }
    attributes.foreach{
      case x if x._1 == "size-min" =>
        val opt = parse(width_height, x._2).get
        if(opt.isDefined){
          size_width_min = Some(opt.get._1)
          size_height_min = Some(opt.get._2)
        }
      case x if x._1 == "size-max" =>
        val opt = parse(width_height, x._2).get
        if(opt.isDefined){
          size_width_max = Some(opt.get._1)
          size_height_max = Some(opt.get._2)
        }
      case x if x._1 == "stretching" =>
        val opt = parse(stretching, x._2).get
        if(opt.isDefined){
          stretching_horizontal = Some(opt.get._1)
          stretching_vertical = Some(opt.get._2)
        }
      case x if x._1 == "proportional" =>
        prop = parse(proportional, x._2).get
      case x if x._1 == "anchor" => anchor = {
        val anch = Anchor.parse(Anchor.anchor, x._2)
        if(anch isEmpty)
          None
        else
          Some(anch.get)
      }
      case x if x._1.matches("description.*") => description = Description.parse(x, style, hierarchyContainer)
      case _ =>
    }
    if(desc nonEmpty)
      description = Description.parse(desc.get, style, hierarchyContainer)
    if(anch nonEmpty)
      anchor = Some(Anchor.parse(Anchor.anchor, anch.get).get)

    /*if parentShape had GeometricModels in 'shapes'-attribute, both the lists (parents and new List of GeometricModels) need to be merged*/
    val geometricModels = parseGeometricModels(geos, style).getOrElse(List())
    val inherited_and_new = {if(shapes isDefined)shapes.get else List()} ::: geometricModels

    /*create the actual shape instance*/
    val newShape = new Shape(name, style, size_width_min, size_height_min, size_width_max, size_height_max,
      stretching_horizontal, stretching_vertical, prop, if(inherited_and_new nonEmpty) Some(inherited_and_new) else None, description, anchor)

    /*include new shape instance in shapeHierarchie*/
    if (extendedStyle.nonEmpty) {
      extendedStyle.reverse.foreach(elem => hierarchyContainer.shapeHierarchy(elem.name, newShape))
    } else {
      hierarchyContainer.shapeHierarchy.newBaseClass(newShape)
    }

    newShape
  }


  /*useful Methodes for generating shape-attribute specific content*/
  private def parseGeometricModels(geoModels:List[GeoModel], parentStyle:Option[Style]) =
    Some(geoModels.map{_.parse(None, parentStyle)}.
      foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r))

  /*parsingRules for special attributes*/
  def proportional:Parser[Option[Boolean]] = "=?".r ~> argument ^^ {
    case prop:String => Some(matchBoolean(prop))
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
