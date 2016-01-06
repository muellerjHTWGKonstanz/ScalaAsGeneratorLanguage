package util

import model.diagram.action.{ActionGroup, ActionInclude, Action}
import model.diagram.edge.Edge
import model.diagram.methodes.{ActionBlock, OnDelete, OnCreate, OnUpdate}
import model.diagram.node.Node
import model.shapecontainer.shape.geometrics.compartment.{Compartment, CompartmentInfo}
import model.Cashe
import model.diagram.Diagram
import model.shapecontainer.connection.Connection
import model.shapecontainer.shape.{ShapeParser, Shape}
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}


/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(cashe: Cashe = Cashe()) extends CommonParserMethodes {
  type diaShape = model.diagram.node.Shape
  type diaConnection = model.diagram.edge.Connection


  /*Style-specific----------------------------------------------------------------------------*/
  private def styleVariable =("""("""+StyleParser.validStyleAttributes.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def styleAttribute = styleVariable ~ arguments ^^ {case v ~ a => (v, a)}
  private def style: Parser[Style] =
    ("style" ~> ident) ~ (("extends" ~> rep(ident <~ ",?".r))?) ~ ("{" ~> rep(styleAttribute)) <~ "}" ^^ {
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, cashe)
    }
  def parseRawStyle(input: String) = parseAll(style, trimRight(input)).get
  /*------------------------------------------------------------------------------------------*/





  /*GeometricModel-specific-------------------------------------------------------------------*/
  private def geoVariable = ("""("""+SprayParser.validGeometricModelVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def geoAttribute = geoVariable ~ (arguments | compartmentinfo ) ^^ {case v ~ a => v+a}

  /**parses a geoModel. first ident is the GeometricModels name, second ident is an optional reference to a style*/
  private def geoModel: Parser[GeoModel] =
    geoIdentifier ~ ((("style" ~> ident)?) <~ "{") ~ rep(geoAttribute) ~ (rep(geoModel) <~ "}") ^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, {
      if(style.isDefined) Some(cashe.styleHierarchy(style.get).data)
      else None }, attr, children, cashe)
  }
  private def geoIdentifier:Parser[String] = "(ellipse|line|polygon|polyline|rectangle|rounded-rectangle|text|wrapped-text)".r ^^ {_.toString}
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
      ShapeParser(name, parent, style, attrs, geos, desc, anch, cashe)
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
      case name ~ style ~ typ ~ anonymousStyle ~ placings => Connection(name, style, typ, anonymousStyle, placings, cashe).get
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
    (("(" ~ "label" ~ ":" ) ~> argument <~ ",") ~
    ("class" ~ ":"  ~> argument <~ ",") ~
    ("method" ~ ":"  ~> ident <~ ")") ^^ {
      case name ~ label ~ className ~ methode =>
        val newAction = Action(name, label, className, methode)
        cashe.actions += name -> newAction
        newAction
    }
  }
  private def possibleActionDefinitionNr2 = {
    ("action" ~> ident) ~
      (("(" ~ "label" ~ ":" ) ~> argument <~ ",") ~
      ("method" ~ ":" ~> ident <~ ",") ~
      ("class" ~ ":" ~> argument <~ ")") ^^ {
      case name ~ label ~ methode ~ className =>
        val newAction = Action(name, label, className, methode)
        cashe.actions += name -> newAction
        newAction
    }
  }

  private def action = {
    possibleActionDefinitionNr1 | possibleActionDefinitionNr2
  }
  private def actionInclude = "include" ~> rep((","?) ~> ident ) <~ ";" ^^ {case includes =>
    ("actionInclude",ActionInclude(includes.map(cashe.actionGroups(_))))}

  private def actions = "actions" ~ "{" ~> (actionInclude?) ~ rep(action) <~ "}" ^^ { case includes ~ actions =>
    ("actions", (includes.get._2 , actions))}
  private def actionGroup = ("actionGroup" ~> ident) ~ ("{" ~> rep(action) <~ "}") ^^ {
    case name ~ acts =>
      val newActionGroup = ActionGroup(name, acts)
      cashe.actionGroups += name -> newActionGroup
      ("actionGroup", newActionGroup)
  }

  private def palette = "palette" ~ ":" ~> argument <~ ";" ^^ {
    case arg => ("palette", arg.toString)
  }
  private def container = "container" ~ ":" ~> argument <~ ";" ^^ {
    case arg => ("container", arg.toString)//TODO eigentlich ecore::EReference
  }
  private def actionBlock = rep(("call"?) ~ "(?!actionGroup)action".r ~> ident) ~ rep(("call"?) ~ "actionGroup" ~> ident) ^^ {
    case actions ~ actionGroups =>
      val acts = actions.map(i => cashe.actions(i))
      val actGrps = actionGroups.map(i => cashe.actionGroups(i))
      ActionBlock(acts, actGrps)
  }
  private def askFor = "askFor" ~ ":" ~> ident ^^ {
    case identifier => identifier +" is a Mock, change this line!!!!"/*TODO this is only a mock, actually ecoreAttribute*/
  }

  private def onCreate = "onCreate" ~ "{" ~> (actionBlock?) ~ (askFor?) <~ "}" ^^ {
    case actblock ~ askfor => ("onCreate", OnCreate(actblock, askfor))//TODO eigentlich ecore::EAttribute
  }
  private def onUpdate = "onUpdate" ~ "{" ~> (actionBlock?) <~ "}" ^^ {
    case actBlock => ("onUpdate", OnUpdate(actBlock))//TODO eigentlich ecore::EAttribute
  }
  private def onDelete = "onDelete" ~ "{" ~> (actionBlock?) <~ "}" ^^ {
      case actBlock => ("onDelete", OnDelete(actBlock))//TODO eigentlich ecore::EAttribute
    }

  private def shapeVALPropertie = ("val" ~> ident) ~ ("->" ~> ident) ^^ {case key ~ value => ("val", key/*TODO eigentlich ecoreAttribute*/ -> value) }
  private def shapeVARPropertie = ("var" ~> ident) ~ ("->" ~> ident) ^^ {case key ~ value => ("var", key -> value) }
  private def shapePropertie = shapeVALPropertie|shapeVARPropertie <~ ",?".r
  private def shapeCompartment = ("nest" ~> ident) ~ ("->" ~> ident) ^^ {case key ~ value => ("nest",key -> value) }
  private def diagramShape:Parser[(String, diaShape)] = ("shape" ~ ":" ~> ident) ~ (("(" ~> rep(shapePropertie|shapeCompartment)/*TODO eigentlicher inhalt*/ <~ ")")?) ^^ {
    case shapeReference ~ propertiesAndCompartments =>
      val referencedShape = cashe.shapeHierarchy.get(shapeReference)
      var vars = Map[String, Text]()
      var vals = Map[String, Text]()
      var nests = Map[String, CompartmentInfo]()
      if(referencedShape isDefined) {
        if(propertiesAndCompartments isDefined) {
          vars = propertiesAndCompartments.get.filter(i => i._1 == "var").map(_._2).map(i => i._1 -> referencedShape.get.textMap.get(i._2)).toMap/*TODO i._1 (at second use) needs to be resolved to an attribute but is not possible at the moment*/
          vals = propertiesAndCompartments.get.filter(i => i._1 == "val").map(_._2).map(i => i._1 -> referencedShape.get.textMap.get(i._2)).toMap
          nests = propertiesAndCompartments.get.filter(i => i._1 == "nest").map(_._2).map(i => i._1 -> referencedShape.get.compartmentMap.get(i._2)).toMap
        }
      }
      ("shape", new diaShape(referencedShape.get, vars, vals, nests))
  }

  private def node:Parser[(String, Node)] = {
    ("node" ~> ident) ~
    ("for" ~> ident) ~
    (("(" ~ "style" ~ ":" ~> ident <~ ")")?) ~
    ("{" ~> rep(diagramShape|palette|container|onCreate|onUpdate|onDelete|actions) <~ "}") ^^ {
      case name ~ ecoreElement ~ style ~ args =>
        //TODO ecoreElement should be resolved ... -> "for Type=[ecore:Class|QualifiedName]"
        val styleOpt = if(style.isDefined)cashe.styleHierarchy.get(style.get) else None
        var shap:Option[diaShape] = None
        var pal:Option[String] = None
        var con:Option[AnyRef] = None
        var onCr:Option[OnCreate] = None
        var onUp:Option[OnUpdate] = None
        var onDe:Option[OnDelete] = None
        var actions:List[Action] = List()
        var actionIncludes:Option[ActionInclude] = None
        args.foreach {
          case i if i._1 == "shape" => shap = Some(i._2.asInstanceOf[diaShape])
          case i if i._1 == "palette" => pal = Some(i._2.asInstanceOf[String])
          case i if i._1 == "container" => con = Some(i._2.asInstanceOf[AnyRef])
          case i if i._1 == "onCreate" => onCr = Some(i._2.asInstanceOf[OnCreate])
          case i if i._1 == "onUpdate" => onUp = Some(i._2.asInstanceOf[OnUpdate])
          case i if i._1 == "onDelete" => onDe = Some(i._2.asInstanceOf[OnDelete])
          case i if i._1 == "actions" =>
            actions = i._2.asInstanceOf[(ActionInclude, List[Action])]._2
            actionIncludes = Some(i._2.asInstanceOf[(ActionInclude, List[Action])]._1)
          case _ =>
        }
        ("node", Node(name, ecoreElement, styleOpt, shap, pal, con, onCr, onUp, onDe, actions, actionIncludes))
    }
  }

  private def diagramConnection = {
    type diaConnection = model.diagram.edge.Connection
    ("connection" ~ ":" ~> ident) ~
      (("(" ~> rep(shapePropertie) <~ ")") ?) ^^ {
      case connectionName ~ properties =>
        val referencedConnection = cashe.connections(connectionName)
        var vars = Map[String, AnyRef]()
        var vals = Map[String, AnyRef]()
        if(properties isDefined) {
          vars = properties.get.filter(i => i._1 == "var").map(_._2).map(i => i._1 -> new Object()).toMap /*TODO new Object (at second use) needs to be resolved to an attribute but is not possible at the moment*/
          vals = properties.get.filter(i => i._1 == "val").map(_._2).map(i => i._1 -> new Object()).toMap /*TODO new Object (at second use) needs to be resolved to an attribute but is not possible at the moment*/
        }
        new diaConnection(referencedConnection,vars, vals)
    }
  }
  private def edge:Parser[(String, Edge)] = {
    type diaConnection = model.diagram.edge.Connection
    ("edge" ~> ident) ~
      ("for" ~> ident) ~
      (("(" ~ "style" ~ ":" ~> ident <~ ")") ?) ~
      ("{" ~> diagramConnection) ~
      ("from" ~ ":" ~> ident) ~
      ("to" ~ ":" ~> ident) ~ (palette?) ~ (container?) ~ (onCreate?) ~ (onUpdate?) ~ (onDelete?) ~ (actions?) <~ "}" ^^ {
      case edgeName ~ ecoreElement ~ styleOpt ~ diaCon ~ from ~ to ~ pal ~ cont ~ oncr ~ onup ~ onde ~ acts =>
        val style = if(styleOpt isDefined)cashe.styleHierarchy.get(styleOpt.get)else None
        val ret_palette = if(pal isDefined)Some(pal.get._2) else None
        val ret_container = if(cont isDefined)Some(cont.get._2) else None
        val onCr = if(oncr isDefined) Some(oncr.get._2) else None
        val onUp = if(onup isDefined) Some(onup.get._2) else None
        val onDe = if(onde isDefined) Some(onde.get._2) else None
        val ret_actions = if(acts isDefined)Some(acts.get._2._2)else None
        val ret_actionIncludes = if(acts isDefined)Some(acts.get._2._1)else None
        ("edge", Edge(edgeName, ecoreElement, style, diaCon, from, to, ret_palette, ret_container, onCr, onUp, onDe, ret_actions.getOrElse(List()), ret_actionIncludes))
    }
  }
  private def nodeOrEdge = node|edge

  def sprayDiagram:Parser[Diagram] = {
      ("diagram" ~> ident) ~
      ("for" ~> ident) ~
      (("(" ~ "style" ~ ":" ~> ident <~ ")")?) ~
      ("{" ~> rep(actionGroup|nodeOrEdge) <~ "}") ^^ {
        case name ~ ecoreElement ~ style ~ arguments =>
          val actionGroups = arguments.filter(i => i._1 == "actionGroup").map(i => i._2.asInstanceOf[ActionGroup].name -> i._2.asInstanceOf[ActionGroup]).toMap
          val nodes = arguments.filter(i => i._1 == "node").map(i => i._2.asInstanceOf[Node].name -> i._2.asInstanceOf[Node]).toMap
          val edges = arguments.filter(i => i._1 == "edge").map(i => i._2.asInstanceOf[Edge].name -> i._2.asInstanceOf[Edge]).toMap

          Diagram(name, actionGroups, nodes, edges, cashe.styleHierarchy(style), ecoreElement/*TODO convert to actual EcoreElement*/)
      }
  }

  def sprayDiagrams = rep(sprayDiagram)
  def parseRawDiagram(e:String) = parseAll(sprayDiagrams, trimRight(e)).get
/*------------------------------------------------------------------------------------------*/

  private def trimRight(s:String) = s.replaceAll("\\/\\/.+", "").split("\n").map(s => s.trim + "\n").mkString
}

object SprayParser{
  val validGeometricModelVariables = List("position", "size", "style", "point", "curve", "align", "id", "compartment")
}
/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ: String, style: Option[Style], attributes: List[String], children: List[GeoModel], hierarchyContainer: Cashe) {

  def parse(parentGeometricModel: Option[GeometricModel], parentStyle:Option[Style]): Option[GeometricModel] = typ match {
    case "ellipse" => Ellipse.parse(this, parentGeometricModel, parentStyle, hierarchyContainer)
    case "line" => Line.parse(this, parentGeometricModel, parentStyle, hierarchyContainer)
    case "polygon" => Polygon.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "polyline" => PolyLine.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "rectangle" => Rectangle.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "rounded-rectangle" => RoundedRectangle.parse(this, parentGeometricModel,parentStyle, hierarchyContainer)
    case "text" => Text.parse(this, parentGeometricModel, DefaultText, parentStyle, hierarchyContainer)
    case "text-wrapped" => Text.parse(this, parentGeometricModel, Multiline, parentStyle, hierarchyContainer)
    case _ => None
  }
}

/**
 * PlacingSketch is a sketch of a placing, only used for parsing a Connection*/
case class PlacingSketch(position:String, shape:GeoModel)

