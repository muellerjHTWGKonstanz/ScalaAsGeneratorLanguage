package util

import model.Diagram
import model.shapecontainer.shape.{ShapeParser, Shape}
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}


/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(diagram: Diagram) extends CommonParserMethodes {
  /*in common usage---------------------------------------------------------------------------*/
  //override def variable: Parser[String] = """\w+([-_]\w+)?\s*""".r ^^ { _.toString }
  /*------------------------------------------------------------------------------------------*/





  /*Style-specific----------------------------------------------------------------------------*/
  private def styleVariable =("""("""+StyleParser.validStyleVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def styleAttribute = styleVariable ~ arguments ^^ {case v ~ a => (v, a)}
  private def style: Parser[Style] =
    ("style" ~> ident) ~ (("extends" ~> rep(ident))?) ~ ("{" ~> rep(styleAttribute)) <~ "}" ^^ {
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, diagram)
    }
  def parseRawStyle(input: String) = parseAll(style, input).get
  /*------------------------------------------------------------------------------------------*/





  /*GeometricModel-specific-------------------------------------------------------------------*/
  def geoVariable = ("""("""+SprayParser.validGeometricModelVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  def geoAttribute = geoVariable ~ arguments ^^ {case v ~ a => v+a}
  private def geometricModels = rep(geoModel) ^^ {
    case a:List[GeoModel] =>
      (for(g <- a)yield{g.parse(None)}).
        foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r)
  }
  /**parses a geoModel first ident is the GeometricModels name, second ident is an optional reference to a style*/
  def geoModel: Parser[GeoModel] =
    geoIdentifier ~ ((("style" ~> ident)?) <~ "{") ~ rep(geoAttribute) ~ (rep(geoModel) <~ "}") ^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, {
      if(style.isDefined) Some(diagram.styleHierarchy(style.get).data)
      else None }, attr, children, diagram)
  }
  private def geoIdentifier:Parser[String] = "(ellipse|line|polygon|polyline|rectangle|rounded-rectangle|text)".r ^^ {_.toString}

  def parseRawGeometricModel(input: String): List[GeometricModel] = {
    parseAll(geometricModels, trimRight(input)).get
  }
  /*------------------------------------------------------------------------------------------*/






  /*Shape-specific----------------------------------------------------------------------------*/
  def shapeVariable = ("""("""+SprayParser.validShapeVariables.map(_+"|").mkString+""")""").r ^^ {_.toString}
  def shapeAttribute = shapeVariable ~ arguments ^^ {case v ~ a => (v, a)}
  def shape:Parser[Shape] = ("shape" ~> ident) ~ (("style" ~> ident)?) ~
    ("{" ~> rep(shapeAttribute)) ~
    (geometricModels <~ "}") ^^
    {case name ~ style ~ attrs ~ geos => ShapeParser(name, style, attrs, geos, diagram)}

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

  private def trimRight(s:String) = s.replaceAll("\\/\\/.+", "").split("\n").map(s => s.trim + "\n").mkString
}

object SprayParser{
  val validShapeVariables = List("size-min", "size-max", "stretching", "proportional", "anchor", "description(\\s*style\\s*[a-zA-ZüäöÜÄÖ]+([-_][a-zA-ZüäöÜÄÖ])*)?\\s*")
  val validGeometricModelVariables = List("position", "size", "style", "point", "curve", "align", "id")
}

/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ: String, style: Option[Style], attributes: List[String], children: List[GeoModel], diagram: Diagram) {
  def parse(parent: Option[GeometricModel]): Option[GeometricModel] = typ match {
    case "ellipse" => Ellipse.parse(this, parent)
    case "line" => Line.parse(this, parent)
    case "polygon" => Polygon.parse(this, parent)
    case "polyline" => PolyLine.parse(this, parent)
    case "rectangle" => Rectangle.parse(this, parent)
    case "rounded-rectangle" => RoundedRectangle.parse(this, parent)
    case "text" => Text.parse(this, parent)
    case _ => None
  }
}