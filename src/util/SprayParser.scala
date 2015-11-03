package util

import model.Diagram
import model.shapecontainer.shape.{ShapeParser, Shape}
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}

import scala.util.parsing.combinator.JavaTokenParsers

trait CommonParserMethodes extends JavaTokenParsers{
  /*basic stuff*/
  def attribute:Parser[(String, String)] = variable ~ argument <~ ",?".r ^^ {case v ~ a => (v.toString,a.toString)}
  def variable:Parser[String] = "[a-züäö]+([-_][a-züäö]+)?\\s*".r  ^^ {_.toString} //<~ "=?\\s*".r
  def argument:Parser[String] = "(([a-züäö]+([-_][a-züäö]+)?)|(\".*\")|([+-]?\\d+(\\.\\d+)?))".r ^^ {_.toString}
  
  /*Some explicit usages*/
  def position:Parser[Option[(Int, Int)]] = "position\\s*\\(\\s*(x=)?".r ~> argument ~ ((",\\s*(y=)?".r ~> argument) <~")") ^^ {
    case xarg ~ yarg => Some((xarg.toInt, yarg.toInt))
    case _ => None}
  def size:Parser[Option[(Int, Int)]] = "size\\s*\\(\\s*(width=)?".r ~> argument ~ (",\\s*(height=)?".r ~> argument) <~ ")" ^^ {
    case width ~ height => Some((width.toInt, height.toInt))
    case _ => None }
  def curve:Parser[Option[(Int, Int)]] = size
  /**
   * takes a String and parses a boolean value out of it -> if string is yes|true|y
   * @param b the stringargument*/
  def matchBoolean(b: String): Boolean = b match {
    case `b` if b toLowerCase() matches "yes|true|y" => true
    //case `b` if b toLowerCase() matches("no|false|n") => false
    case _ => false
  }
}
/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(diagram: Diagram) extends CommonParserMethodes {
  /*in common usage---------------------------------------------------------------------------*/
  //override def variable: Parser[String] = """\w+([-_]\w+)?\s*""".r ^^ { _.toString }
  def argument_classic: Parser[String] = """\s*\=\s*""".r ~> argument^^ { _.toString }
  //def argument_advanced_explicit:Parser[List[(String, String)]] = "[\\(\\{]".r ~> rep(attribute) <~ "[\\)\\}]".r ^^ {
  //  case attrs: List[(String, String)] => attrs
  //}
  def argument_advanced_explicit: Parser[String] = """\((\w+([-_]\w+)?\s*=\s*([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?[\s\n]*)+\)""".r ^^ { _.toString }
  def argument_advanced_implicit: Parser[String] = """\((([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?\s*)+\)""".r ^^ { _.toString }
  def arguments: Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit
  def attribute_string: Parser[String] = variable ~ arguments ^^ { case v ~ arg => v + arg }
  def attributePair: Parser[(String, String)] = variable ~ arguments ^^ { case v ~ a => (v, a) }
  /*------------------------------------------------------------------------------------------*/





  /*Style-specific----------------------------------------------------------------------------*/
  private def style: Parser[Style] =
    ("style" ~> ident) ~ (("extends" ~> rep(ident))?) ~ ("{" ~> rep(attributePair)) <~ "}" ^^ {
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, diagram)
    }
  def parseRawStyle(input: String) = parseAll(style, input).get
  /*------------------------------------------------------------------------------------------*/





  /*GeometricModel-specific-------------------------------------------------------------------*/
  private def geometricModels = rep(geoModel) ^^ {
    case a:List[GeoModel] =>
      (for(g <- a)yield{g.parse(None)}).
        foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r)
  }
  /**parses a geoModel first ident is the GeometricModels name, second ident is an optional reference to a style*/
  private def geoModel: Parser[GeoModel] = geoIdentifier ~ ((("style" ~> ident)?) <~ "{") ~ rep(attribute_string) ~ (rep(geoModel) <~ "}") ^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, {
      if(style.isDefined) Some(diagram.styleHierarchy(style.get).data)
      else None }, attr, children, diagram)
  }
  private def geoIdentifier:Parser[String] = "(ellipse|line|polygon|polyline|rectangle|roundedrectangle|text)".r ^^ {_.toString}

  def parseRawGeometricModel(input: String): List[GeometricModel] = {
    parseAll(geometricModels, trimRight(input)).get
  }
  /*------------------------------------------------------------------------------------------*/






  /*Shape-specific----------------------------------------------------------------------------*/
  private def shape:Parser[Shape] = ("shape" ~> ident) ~ (("style" ~> ident)?) ~
    ("{" ~> rep(attributePair)) ~
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
    case "roundedrectangle" => RoundedRectangle.parse(this, parent)
    case "text" => Text.parse(this, parent)
    case _ => None
  }
}
