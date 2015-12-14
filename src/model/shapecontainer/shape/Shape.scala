package model.shapecontainer.shape

import model.shapecontainer.shape.geometrics.compartment.CompartmentInfo
import model.{ClassHierarchy, Cashe}
import model.shapecontainer.ShapeContainerElement
import model.shapecontainer.shape.anchor.Anchor
import model.shapecontainer.shape.anchor.Anchor.AnchorType
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}
import util.{GeoModel, CommonParserMethodes}

/**
 * Created by julian on 29.09.15.
 * The actual representation of a shape class,
 * will hold all the relevant attributes
 *
 * @param name = id
 * @param parentShapes are the shapes that this new shape will inherit its attributes of
 * @param geos is a list of GeoModels kind of like sketch-GeometricModels, which will be converted into real GeometricModels inside the constructor
 */
class Shape(val name:String = "no name",
                  val style:Option[Style]                   = None,
                  val size_width_min:Option[Int]            = None, /*from ShapeLayout*/
                  val size_height_min:Option[Int]           = None, /*from ShapeLayout*/
                  val size_width_max:Option[Int]            = None, /*from ShapeLayout*/
                  val size_height_max:Option[Int]           = None, /*from ShapeLayout*/
                  val stretching_horizontal:Option[Boolean] = None, /*from ShapeLayout*/
                  val stretching_vertical:Option[Boolean]   = None, /*from ShapeLayout*/
                  val proportional:Option[Boolean]          = None, /*from ShapeLayout*/

                  parentShapes:Option[List[GeometricModel]] = None,
                  parentTextMap:Option[Map[String, Text]]   = None, /*necessary addition*/
                  parentCompartmentMap:Option[Map[String, CompartmentInfo]]= None, /*necessary addition*/
                  geos:List[GeoModel]                       = List(),

                  val description:Option[Description]       = None,
                  val anchor:Option[AnchorType]             = None,
                  val extendedShape:List[Shape] = List()) extends ShapeContainerElement{

  /*if parentShape had GeometricModels in 'shapes'-attribute, both the lists (parents and new List of GeometricModels) need to be merged*/
  val shapes = {
    val geometricModels = parseGeometricModels(geos, style).getOrElse(List())
    val inherited_and_new_geometrics = {if(parentShapes isDefined) parentShapes.get else List()} ::: geometricModels
    if(inherited_and_new_geometrics nonEmpty)Some(inherited_and_new_geometrics)else None
  }

  /*if parentShape had TextOutputFields (Text) and if new TextFields were parsed, create a new Map[String, Text]*/
  /*first check for new TextOutputs*/
  val textMap = {
    var ret = parentTextMap
    if(shapes isDefined) {
      var texts = shapes.get.filter(i => i.isInstanceOf[Text]).map(i => i.asInstanceOf[Text].id -> i.asInstanceOf[Text]).toMap
      /*now check for old TextOutputs*/
      if (parentTextMap.isDefined)
        parentTextMap.get.foreach(i => texts += i)
      if (texts nonEmpty) ret = Some(texts)
    }
    ret
  }

  val compartmentMap = {
    /*if parentShape had CompartmentInfos and if new CompartmentInfos were parsed, create a new Map[String, CompartmentInfo]*/
    /*first check for new CompartmentInfo*/
    val comparts = if (shapes isDefined) {
      rCompartment(shapes.get)
    } else List[CompartmentInfo]()
    if(comparts nonEmpty)
      Some(comparts.map(i => i.compartment_id.get -> i).toMap)
    else None
  }


  /*useful Methodes */
  override def toString = "Shape("+name +
                       /*"; style: "                  +*/", " + style +
                       /*"; size_width_min: "         +*/", " + size_width_min +
                       /*"; size_height_min: "        +*/", " + size_height_min +
                       /*"; size_width_max: "         +*/", " + size_width_max +
                       /*"; size_height_max: "        +*/", " + size_height_max +
                       /*"; stretching_horizontal: "  +*/", " + stretching_horizontal +
                       /*"; stretching_vertical: "    +*/", " + stretching_vertical +
                       /*"; proportional: "           +*/", " + proportional +
                       /*"; shapes: "                 +*/", " + shapes +
                       /*"; tests: "                  +*/", " + textMap +
                       /*"; compartments: "           +*/", " + compartmentMap +
                       /*"; description: "            +*/", " + description +
                       /*"; anchor: "                 +*/", " + anchor +
                       /*"; parentShapes: "           +*/", " + extendedShape +")"

  /*for generating shape-attribute specific content*/
  private def parseGeometricModels(geoModels:List[GeoModel], parentStyle:Option[Style]) =
    Some(geoModels.map{_.parse(None, parentStyle)}.
      foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r))

  /*recursively searches for Compartments in the geometricModels*/
  private def rCompartment(g:List[GeometricModel], compartments:List[CompartmentInfo] = List[CompartmentInfo]()):List[CompartmentInfo] = {
    var ret:List[CompartmentInfo] = compartments
    g foreach{
      case e: Ellipse if e.compartmentInfo.isDefined =>
        ret = e.compartmentInfo.get :: ret
      case e: Rectangle if e.compartmentInfo.isDefined =>
        ret = e.compartmentInfo.get :: ret
      case _ =>
    }
    g foreach {
      case e: Wrapper =>
        ret = ret ::: rCompartment(e.children, ret) case _ =>
    }
    ret
  }
}

object ShapeParser extends CommonParserMethodes{
  val validShapeVariables = List("size-min", "size-max", "stretching", "proportional", "anchor", "description(\\s*style\\s*[a-zA-ZüäöÜÄÖ]+([-_][a-zA-ZüäöÜÄÖ])*)?\\s*")

  def apply(name:String, parents:Option[List[String]], style:Option[String], attributes:List[(String, String)], geos:List[GeoModel], description:Option[(String, String)], anchor:Option[String], hierarchyContainer:Cashe) =
    parse(name, parents, style, attributes, geos, description, anchor, hierarchyContainer)

  def parse(name:String,
            parentShapes:Option[List[String]],
            styleArgument:Option[String],
            attributes:List[(String, String)],
            geos:List[GeoModel],
            desc:Option[(String, String)],
            anch:Option[String],
            hierarchyContainer:Cashe):Shape = {

    val parents = if(parentShapes isDefined) parentShapes.get else List()
    var extendedShapes:List[Shape] = List[Shape]()
    if(parents.nonEmpty)
      parents.foreach{parent => {
        val parentName = parent.trim //trim just to make sure, could probably be removed
        if(hierarchyContainer.shapeHierarchy.contains(parentName))
          extendedShapes = hierarchyContainer.shapeHierarchy(parentName).data :: extendedShapes
      }
      }

    /*mapping*/
    /** relevant is a help-methode, which shortens the actual call to mostRelevant of ClassHierarchy by ensuring the collection-parameter
      * relevant speaks for the hierarchical context -> "A extends B, C" -> C is most relevant */
    def relevant[T](f: Shape => Option[T]) = ClassHierarchy.mostRelevant(extendedShapes) {f}

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
    val textMap:Option[Map[String, Text]]     = relevant {_.textMap}
    val compartmentMap:Option[Map[String, CompartmentInfo]]= relevant {_.compartmentMap}

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

    /*create the actual shape instance*/
    val newShape = new Shape(name, style, size_width_min, size_height_min, size_width_max, size_height_max,
      stretching_horizontal, stretching_vertical, prop, shapes, textMap, compartmentMap, geos, description, anchor)

    /*include new shape instance in shapeHierarchie*/
    if (extendedShapes.nonEmpty) {
      extendedShapes.reverse.foreach(elem => hierarchyContainer.shapeHierarchy(elem.name, newShape))
    } else {
      hierarchyContainer.shapeHierarchy.newBaseClass(newShape)
    }
    newShape
  }


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
