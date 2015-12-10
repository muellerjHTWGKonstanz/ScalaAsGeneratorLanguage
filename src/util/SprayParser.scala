package util

import model.diagram.action.{ActionGroup, LocalActionGroup, ActionInclude, Action}
import model.diagram.methodes.{OnDelete, OnCreate, OnUpdate}
import model.diagram.node.{ShapeCompartment, ShapeProperty, Node}
import model.{ClassHierarchy, HierarchyContainer}
import model.diagram.{node, Diagram}
import model.shapecontainer.connection.Connection
import model.shapecontainer.shape.{ShapeParser, Shape}
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}


/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(hierarchyContainer: HierarchyContainer =
                  HierarchyContainer(new ClassHierarchy[Style](new Style(name = "rootStyle")),
                                     new ClassHierarchy[Shape](new Shape(name = "rootShape"))
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
    ("class" ~> argument) ~
    ("methode" ~> ident <~ ")") ^^ {
      case name ~ label ~ className ~ methode => Action(name, label, className, methode)
    }
  }
  private def possibleActionDefinitionNr2 = {
    ("action" ~> ident) ~
      (("(" ~ "label") ~> argument) ~
      ("methode" ~> ident) ~
      ("class" ~> argument <~ ")") ^^ {
      case name ~ label ~ methode ~ className => Action(name, label, className, methode)
    }
  }

  private def action = {
    possibleActionDefinitionNr1 | possibleActionDefinitionNr2
  }
  private def actionInclude = "include" ~> rep((","?) ~> ident ) <~ (";"?) ^^ {case includes => ("actionInclude",ActionInclude(includes))}

  private def actions = "actions" ~ "{" ~> actionInclude ~ rep(action) <~ "}" ^^ { case includes ~ actions => ("actions", (includes._2, actions))}
  private def actionGroup = ("actionGroup" ~> ident) ~ ("{" ~> rep(action) <~ "}") ^^ {
    case name ~ acts => ActionGroup(name, acts)
  }
  private def globalActionGroups=  rep(actionGroup)

  private def palette = "palette" ~ ":" ~> argument <~ ";" ^^ {case arg => ("palette", arg.toString)}
  private def container = "container" ~ ":" ~> argument <~ ";" ^^ {case arg => ("container", arg.toString)} //TODO eigentlich ecore::EReference

  private def actionBlock = rep(("call"?) ~ "action" ~> ident) ~ rep(("call"?) ~ "actionGroup" ~> ident) ^^ {
    case actions ~ actionGroups => (actions, actionGroups)
  }
  private def askFor = "askFor" ~ ":" ~> ident ^^ {case identifier => "Object Mock, change this line!!!!"/*TODO this is only a mock, actually ecoreAttribute*/}
  private def onCreate = "onCreate" ~ "{" ~> actionBlock ~ askFor <~ "}" ^^ {case actblock ~ askfor => ("onCreate", (actblock, askfor))} //TODO eigentlich ecore::EAttribute
  private def onUpdate = "onUpdate" ~ "{" ~> actionBlock <~ "}" ^^ {case arg => ("onUpdate", arg)} //TODO eigentlich ecore::EAttribute
  private def onDelete = "onDelete" ~ "{" ~> actionBlock <~ "}" ^^ {case arg => ("onDelete", arg)} //TODO eigentlich ecore::EAttribute

  private def properties = rep(ident)/*TODO not right */
  private def compartment = ("nest" ~> ident) ~ ("->" ~> ident) ^^ {case id ~ ident => /*TODO ask Markus why 2 IDs??? only need one to refer to an existing compartmentInfo*/}
  private def diagramShape = ("shape" ~ ":" ~> ident) ~ (("(" ~> properties/*TODO eigentlicher inhalt*/ <~ ")")?) ^^ {
    case shapeReference ~ propertiesAndCompartments =>
      val referencedShape = hierarchyContainer.shapeHierarchy.get(shapeReference)
      if(referencedShape isDefined) {
        if(propertiesAndCompartments isDefined) {
          var props: List[ShapeProperty] = List()
          var compart: List[ShapeCompartment] = List()
          props = propertiesAndCompartments.get.map(i => {
            val compMap = referencedShape.get.compartmentMap
            if (compMap isDefined) {
              val compartmentOption = compMap.get.get(i)
              if(compartmentOption isDefined)
                new ShapeProperty()
            }
          })
          if (referencedShape.isDefined)
            new node.Shape(referencedShape.get,)
        }
      }
      None
  }

  private def node = {
    type diaShape = model.diagram.node.Shape
    ("node" ~> ident) ~
    ("for" ~> ident) ~
    (("(" ~ "style" ~ ":" ~> ident <~ ")")?) ~
    ("{" ~> rep(/*TODO diagramShape |*/palette|container|onCreate|onUpdate|onDelete|actions|actionInclude) <~ "}") ^^ {
      case name ~ ecoreElement ~ style ~ args =>
        //TODO for Type=[ecore:Class|QualifiedName]
        val styleOpt = if(style.isDefined)hierarchyContainer.styleHierarchy.get(style.get) else None
        var shap:Option[diaShape] = None
        var pal:Option[String] = None
        var con:Option[String] = None
        var onCr:Option[OnCreate] = None
        var onUp:Option[OnUpdate] = None
        var onDe:Option[OnDelete] = None
        var actions:List[Action] = List()
        var actionIncludes:Option[ActionInclude] = None
        args.foreach {
          case i if i._1 == "palette" => pal = i._2.asInstanceOf
          case i if i._1 == "container" => con = i._2.asInstanceOf
          case i if i._1 == "onCreate" => onCr = i._2.asInstanceOf
          case i if i._1 == "onUpdate" => onUp = i._2.asInstanceOf
          case i if i._1 == "onDelete" => onDe = i._2.asInstanceOf
          case i if i._1 == "actions" => {
            actions = i._2.asInstanceOf[(ActionInclude, List[Action])]._2
            actionIncludes = Some(i._2.asInstanceOf[(ActionInclude, List[Action])]._1)
          }
          case i:(String, diaShape) => shap = Some(i._2)
          case _ =>
        }
        Node(name, null, styleOpt, shap, pal, con, onCr, onUp, onDe, actions, actionIncludes)
    }
  }

  private def edge = ???
  private def nodesOrEdges = rep(node|edge) ^^ ???

  def sprayDiagram:Parser[Diagram] = {
      ("diagram" ~> ident) ~
      ("for" ~> ident) ~
      (("(" ~ "style" ~ ":" ~> ident <~ ")")?) ~
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

  def parse(parentGeometricModel: Option[GeometricModel], parentStyle:Option[Style], ancestorShape:Shape): Option[GeometricModel] = typ match {
    case "ellipse" => Ellipse.parse(this, parentGeometricModel, parentStyle, hierarchyContainer, ancestorShape)
    case "line" => Line.parse(this, parentGeometricModel, parentStyle, hierarchyContainer)
    case "polygon" => Polygon.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "polyline" => PolyLine.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "rectangle" => Rectangle.parse(this, parentGeometricModel,parentStyle, hierarchyContainer, ancestorShape)
    case "rounded-rectangle" => RoundedRectangle.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "text" => Text.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case _ => None
  }
}

/**
 * PlacingSketch is a sketch of a placing, only used for parsing a Connection*/
case class PlacingSketch(position:String, shape:GeoModel)

