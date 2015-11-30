package util

import model.diagram.action.{LocalActionGroup, ActionInclude, GlobalActionGroup, Action}
import model.diagram.node.Node
import model.{ClassHierarchy, HierarchyContainer}
import model.diagram.Diagram
import model.shapecontainer.connection.Connection
import model.shapecontainer.shape.{ShapeParser, Shape}
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}


/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(hierarchyContainer: HierarchyContainer =
                  HierarchyContainer(new ClassHierarchy[Style](new Style(name = "root")),
                                     new ClassHierarchy[Shape](new Shape(name = "root"))
                  )) extends CommonParserMethodes {


  /*Style-specific----------------------------------------------------------------------------*/
  private def styleVariable =("""("""+StyleParser.validStyleAttributes.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def styleAttribute = styleVariable ~ arguments ^^ {case v ~ a => (v, a)}
  private def style: Parser[Style] =
    ("style" ~> ident) ~ (("extends" ~> rep(ident <~ ",?".r))?) ~ ("{" ~> rep(styleAttribute)) <~ "}" ^^ {
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, hierarchyContainer)
    }
  def parseRawStyle(input: String) = parseAll(style, trimRight(input)).get
  /*------------------------------------------------------------------------------------------*/





  /*GeometricModel-specific-------------------------------------------------------------------*/
  private def geoVariable = ("""("""+SprayParser.validGeometricModelVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def geoAttribute = geoVariable ~ (arguments | compartmentinfo ) ^^ {case v ~ a => v+a}


  @Deprecated
  private def geometricModels = rep(geoModel) ^^ {
    case a:List[GeoModel] =>
      (for(g <- a)yield{g.parse(None, None)}).
        foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r)
  }


  /**parses a geoModel first ident is the GeometricModels name, second ident is an optional reference to a style*/
  private def geoModel: Parser[GeoModel] =
    geoIdentifier ~ ((("style" ~> ident)?) <~ "{") ~ rep(geoAttribute) ~ (rep(geoModel) <~ "}") ^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, {
      if(style.isDefined) Some(hierarchyContainer.styleHierarchy(style.get).data)
      else None }, attr, children, hierarchyContainer)
  }
  private def geoIdentifier:Parser[String] = "(ellipse|line|polygon|polyline|rectangle|rounded-rectangle|text)".r ^^ {_.toString}

  private def parseRawGeometricModel(input: String): List[GeometricModel] = {
    parseAll(geometricModels, trimRight(input)).get
  }
  /*------------------------------------------------------------------------------------------*/






  /*Shape-specific----------------------------------------------------------------------------*/
  private def shapeVariable = ("""("""+ShapeParser.validShapeVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def shapeAttribute = shapeVariable ~ arguments ^^ {case v ~ a => (v, a)}
  private def descriptionAttribute = "description\\s*(style ([a-zA-ZüäöÜÄÖ][-_]?)+)?".r ~ argument_wrapped ^^ {case des ~ arg => (des, arg)}
  private def anchorAttribute = "anchor" ~> arguments ^^ {_.toString}

  private def shape:Parser[Shape] =
    ("shape" ~> ident) ~
    (("extends" ~> rep(("(?!style)".r ~> ident)<~ ",?".r))?) ~
    (("style" ~> ident)?) ~
    ("{" ~> rep(shapeAttribute)) ~
    rep(geoModel) ~
    (descriptionAttribute?) ~
    (anchorAttribute?) <~ "}" ^^
    {case name ~ parent ~ style ~ attrs ~ geos ~ desc ~ anch =>
      val pStyle = if(style isDefined)hierarchyContainer.styleHierarchy.get(style.get) else None
      ShapeParser(name, parent, style, attrs, geos, desc, anch, hierarchyContainer)
    }

  private def shapes = rep(shape)

  def parseRawShape(input:String):List[Shape] = parseAll(shapes, trimRight(input)).get
  def parseRawShape:List[Shape] = {
    import scala.swing.FileChooser
    val chooser = new FileChooser()
    if(chooser.showOpenDialog(null) == FileChooser.Result.Approve)
      parseRawShape(scala.io.Source.fromFile(chooser.selectedFile).mkString)
    else
      List()
  }
  /*------------------------------------------------------------------------------------------*/



  /*Connection-Specific-----------------------------------------------------------------------*/
  private def c_type = "connection-type\\s*=\\s*".r ~> "(freeform|manhatten)".r
  private def c_placing = ("placing\\s*\\{".r ~> ("position" ~> arguments)) ~ (geoModel <~ "}") ^^ {
    case posi ~ geo => PlacingSketch(posi, geo)
  }
  private def c_style = "style\\s*".r ~ arguments ^^ {_.toString()}

  private def connection:Parser[Connection] =
    ("connection" ~> ident) ~
    (("style" ~> ident)?) ~
    ("{" ~> (c_type?)) ~
    (c_style?) ~
    rep(c_placing) <~ "}" ^^ {
      case name ~ style ~ typ ~ anonymousStyle ~ placings => Connection(name, style, typ, anonymousStyle, placings, hierarchyContainer).get
    }
  private def connections = rep(connection)

  def parseRawConnection(input:String) = parseAll(connections, trimRight(input)).get
  def parseRawConnection:List[Connection] = {
    import scala.swing.FileChooser
    val chooser = new FileChooser()
    if(chooser.showOpenDialog(null) == FileChooser.Result.Approve)
      parseRawConnection(scala.io.Source.fromFile(chooser.selectedFile).mkString)
    else
      List()
  }
  /*------------------------------------------------------------------------------------------*/




  /*Diagram-Specific--------------------------------------------------------------------------*/
  private def possibleActionDefinitionNr1 = {
    ("action" ~> ident) ~
    (("(" ~ "label") ~> argument) ~
    ("implementation" ~> attributeAsString <~ ")") ^^ {
      case name ~ label ~ realizedBy => Action(name, label, realizedBy)
    }
  }
  private def possibleActionDefinitionNr2 = {
    ("action" ~> ident) ~
      (("(" ~ "implementation") ~> argument) ~
      ("label" ~> attributeAsString <~ ")") ^^ {
      case name ~ realizedBy ~ label  => Action(name, label, realizedBy)
    }
  }
  private def action = {
    possibleActionDefinitionNr1 | possibleActionDefinitionNr2
  }
  private def actionGroup = ("actionGroup" ~> ident) ~ ("{" ~> rep(action) <~ "}") ^^ {
    case name ~ acts => GlobalActionGroup(name, acts)
  }
  private def actionInclude = rep((","?) ~ "include" ~> ident <~ ";") ^^ {case includes => ActionInclude(includes)}
  private def localActionGroup = "actions" ~ ":" ~ "{" ~> rep(actionInclude) ~ rep(action) <~ "}" ^^ {
    case includes ~ actions => ("localActionGroup", LocalActionGroup(includes, actions))
  }
  private def globalActionGroups=  rep(actionGroup)

  private def palette = "palette" ~ ":" ~> argument <~ ";" ^^ {case arg => ("palette", arg.toString)}
  private def container = "container" ~ ":" ~> argument <~ ";" ^^ {case arg => ("container", arg.toString)} //TODO eigentlich ecore::EReference
  private def onCreate = "onCreate" ~ ":" ~ "askFor" ~> argument <~ ";" ^^ {case arg => ("onCreate", arg.toString)} //TODO eigentlich ecore::EAttribute

  private def node = {
    "node" ~> ident ~
    //TODO for Type=[ecore:Class|QualifiedName]
    ("{" ~> rep(/*TODO shape |*/palette|container|onCreate|localActionGroup) <~ "}") ^^ {
      case name ~ args =>
        var shap:Option[model.diagram.node.Shape] = None
        var actionGroup:Option[LocalActionGroup]=None
        var pal:Option[String] = None
        var con:Option[String] = None
        var oncr:Option[String] = None
        args.foreach {
          case i if i._1 == "palette" => pal = i._2.asInstanceOf
          case i if i._1 == "container" => con = i._2.asInstanceOf
          case i if i._1 == "onCreate" => oncr = i._2.asInstanceOf
          case i if i._1 == "localActionGroup" => actionGroup = Some(i._2.asInstanceOf)
          case i:(String, model.diagram.node.Shape) => shap = Some(i._2)
          case _ =>
        }
        Node(name, None, actionGroup, None, pal.getOrElse(""), con.getOrElse(""), oncr.getOrElse(""))
    }
  }

  private def edge = ???
  private def nodesOrEdges = rep(node|edge)
  def sprayDiagram:Parser[Diagram] = {
      ("diagram" ~> ident) ~
       (("style" ~> ident)?) ~
      //TODO for modelType=[ecore:Class|QualifiedName]
      globalActionGroups ~
      nodesOrEdges ^^ {
        case name ~ style ~ actionGroups ~ nodesAndEdges => Diagram(actionGroups.map(i => i.name -> i).toMap, nodesAndEdges, hierarchyContainer.styleHierarchy(style), ""/*TODO*/)
      }
  }


  /*------------------------------------------------------------------------------------------*/

  private def trimRight(s:String) = s.replaceAll("\\/\\/.+", "").split("\n").map(s => s.trim + "\n").mkString
}

object SprayParser{
  val validGeometricModelVariables = List("position", "size", "style", "point", "curve", "align", "id", "compartment")
}
/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ: String, style: Option[Style], attributes: List[String], children: List[GeoModel], hierarchyContainer: HierarchyContainer) {
  def parse(parent: Option[GeometricModel], parentStyle:Option[Style]): Option[GeometricModel] = typ match {
    case "ellipse" => Ellipse.parse(this, parent, parentStyle, hierarchyContainer)
    case "line" => Line.parse(this, parent, parentStyle, hierarchyContainer)
    case "polygon" => Polygon.parse(this, parent,parentStyle, hierarchyContainer)
    case "polyline" => PolyLine.parse(this, parent,parentStyle, hierarchyContainer)
    case "rectangle" => Rectangle.parse(this, parent,parentStyle, hierarchyContainer)
    case "rounded-rectangle" => RoundedRectangle.parse(this, parent,parentStyle, hierarchyContainer)
    case "text" => Text.parse(this, parent,parentStyle, hierarchyContainer)
    case _ => None
  }
}

/**
 * PlacingSketch is a sketch of a placing, only used for parsing a Connection*/
case class PlacingSketch(position:String, shape:GeoModel)

