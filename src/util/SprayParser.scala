package util

import model.Diagram
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}

import scala.util.Random
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by julian on 23.10.15.
 * offers functions like parseRawShape/Style, which parses style or shape strings to instances
 */
class SprayParser(diagram: Diagram) extends JavaTokenParsers {
  /*in common usage*/
  private def variable: Parser[String] = """\w+([-_]\w+)?\s*""".r ^^ { _.toString }
  private def argument_classic: Parser[String] = """\s*\=\s*(([a-z]+)|(\".*\")|([+-]?\d+(\.\d+)?))""".r ^^ { _.toString }
  private def argument_advanced_explicit: Parser[String] = """\((\w+([-_]\w+)?\s*=\s*([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?[\s\n]*)+\)""".r ^^ { _.toString }
  private def argument_advanced_implicit: Parser[String] = """\((([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?\s*)+\)""".r ^^ { _.toString }
  private def argument: Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit
  private def attribute: Parser[String] = variable ~ argument ^^ { case v ~ arg => v + arg }
  private def attributePair: Parser[(String, String)] = variable ~ argument ^^ { case v ~ a => (v, a) }


  /*Style-specific*/
  private def style: Parser[Style] =
    ("style" ~> ident) ~ (("extends" ~> rep(ident))?) ~ ("{" ~> rep(attributePair)) <~ "}" ^^ {
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, diagram)
    }
  def parseRawStyle(input: String) = parseAll(style, input).get


  /*Shape-specific*/
  private def geometricModels = rep(geoModel) ^^ {
    case a:List[GeoModel] =>
      (for(g <- a)yield{g.parse(None)}).
        foldLeft(List[GeometricModel]())((r, c:Option[GeometricModel])=>if(c.isDefined)r.::(c.get) else r)
  }
  /**parses a geoModel first ident is the GeometricModels name, second ident is an optional reference to a style*/
  private def geoModel: Parser[GeoModel] = ident ~ ((("style" ~> ident)?) <~ "{") ~ rep(attribute) ~ (rep(geoModel) <~ "}") ^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, {
      if(style.isDefined) Some(diagram.styleHierarchy(style.get).data)
      else None }, attr, children, diagram)
  }
  def parseRawShape(input: String) = {
    parseAll(geometricModels, input.replaceAll("\\/\\/.+", "").split("\n").slice(1, input.lines.length - 1).
      map(s => s.trim + "\n").
      mkString).get
  }
}

/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ: String, style: Option[Style], attributes: List[String], children: List[GeoModel], diagram: Diagram) {
  def parse(parent: Option[GeometricModel]): Option[GeometricModel] = typ match {
    case "ellipse" => Ellipse.parse(this, diagram, None)
    case "line" => Line.parse(this, diagram, None)
    case "polygon" => Polygon.parse(this, diagram, None)
    case "polyline" => PolyLine.parse(this, diagram, None)
    case "rectangle" => Rectangle.parse(this, diagram, None)
    case "roundedrectangle" => RoundedRectangle.parse(this, diagram, None)
    case "text" => Text.parse(this, diagram, None)
    case _ => None
  }
}
