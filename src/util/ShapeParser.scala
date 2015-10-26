package util

import model.Diagram
import model.shapecontainer.shape.geometrics._
import model.style.{StyleParser, Style}

import scala.util.Random
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by julian on 23.10.15.
 * will be able to parse a shape-string to a Shape instance
 */
class ShapeParser(diagram:Diagram) extends JavaTokenParsers{
  def variable:Parser[String] = """\w+([-_]\w+)?\s?""".r ^^ {_.toString}
  def argument_classic:Parser[String] = """=\s?(\d+|\w+)""".r ^^ {_.toString}
  def argument_advanced_explicit:Parser[String] ="""\((\w+((-|_)\w+)?\s?=\s?(\w+|\d+),?\s?)+\)""".r ^^ {_.toString}
  def argument_advanced_implicit:Parser[String] ="""\(((\w+|\d+),?\s?)+\)""".r ^^ {_.toString}
  def argument:Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit
  def attribute:Parser[String] = variable ~ argument ^^ {case v ~ arg => v+arg}
  def attributePair:Parser[(String,String)] = variable ~ argument ^^ {case v ~ a => (v,a)}

  /*Style-specific*/
  def style = styleDeclaration | styleDefinition
  def styleDefinition:Parser[Style] = ("style (" ~> attributePair) <~ ")" ^^ {case attr => StyleParser("noname"+Random.nextString(100), List(), attr +: List(), diagram)}
  def styleDeclaration = styleDeclaration_extends | styleDeclaration_baseClass
  def styleDeclaration_extends:Parser[Style] =
    ("style" ~> ident) ~ ("extends" ~> rep(ident)) ~ ("{" ~> rep(attributePair)) <~ "}" ^^{
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, diagram)
    }
  def styleDeclaration_baseClass:Parser[Style] = {
    (("style" ~> ident) <~ "{") ~ (rep(attributePair) <~ "}") ^^ {
      case name ~ attributes => StyleParser(name, List[String](), attributes, diagram)
    }
  }

  private def geoModel:Parser[GeoModel] = (ident <~ "{") ~ rep(attribute) ~ (rep(geoModel) <~ "}")^^ {
    case name ~ attr ~ children => GeoModel(name, attr, children, diagram)
  }
  private def shape = rep(geoModel)

  def parseRawShape(input:String) = {
    parseAll(shape, input.replaceAll("\\/\\/.+","").split("\n").slice(1, input.lines.length -1).
      map(s => s.trim +"\n").
      mkString).get
  }
}

/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ:String, attributes:List[String], children:List[GeoModel], diagram: Diagram){
  def parse(parent:Option[GeometricModel]):Option[GeometricModel] = typ match {
      case "ellipse"    => Ellipse.parse(this, diagram, None)
      case "line"       => Line.parse(this, diagram, None)
      case "polygon"    => Polygon.parse(this, diagram, None)
      case "polyline"   => PolyLine.parse(this, diagram, None)
      case "rectangle"  => Rectangle.parse(this, diagram, None)
      case "roundedrectangle" => RoundedRectangle.parse(this, diagram, None)
      case "text"       => Text.parse(this, diagram, None)
  }
}
