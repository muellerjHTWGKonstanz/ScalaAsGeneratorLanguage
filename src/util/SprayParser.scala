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
class SprayParser(diagram:Diagram) extends JavaTokenParsers{
   private def variable:Parser[String] = """\w+([-_]\w+)?\s?""".r ^^ {_.toString}
   private def argument_classic:Parser[String] = """\s?\=\s?(([a-z]+)|(\".*\")|([+-]?\d+(\.\d+)?))""".r ^^ {_.toString}
   private def argument_advanced_explicit:Parser[String] ="""\((\w+([-_]\w+)?\s*=\s*([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?\s?)+\)""".r ^^ {_.toString}
   private def argument_advanced_implicit:Parser[String] ="""\((([a-zA-Z]+|(\".*\")|([+-]?\d+(\.\d+)?)),?\s?)+\)""".r ^^ {_.toString}
   private def argument:Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit

   private def attribute:Parser[String] = variable ~ argument ^^ {case v ~ arg => v+arg}
   private def attributePair:Parser[(String,String)] = variable ~ argument ^^ {case v ~ a => (v,a)}

  /*Style-specific*/
   private def style =styleDeclaration | styleDefinition
   private def styleDefinition:Parser[Style] = ("style (" ~> attributePair) <~ ")" ^^ {case attr => StyleParser("noname"+Random.nextString(100), List(), attr +: List(), diagram)}
   private def styleDeclaration = styleDeclaration_extends | styleDeclaration_baseClass
   private def styleDeclaration_extends:Parser[Style] =
    ("style" ~> ident) ~ ("extends" ~> rep(ident)) ~ ("{" ~> rep(attributePair)) <~ "}" ^^{
      case name ~ parents ~ attributes => StyleParser(name, parents, attributes, diagram)
    }
   private def styleDeclaration_baseClass:Parser[Style] = {
    (("style" ~> ident) <~ "{") ~ (rep(attributePair) <~ "}") ^^ {
      case name ~ attributes => StyleParser(name, List[String](), attributes, diagram)
    }
  }
  def parseRawStyle(input:String) = parseAll(style, input).get

  /*Shape-specific*/
   private def shape = rep(geoModel)
   private def geoModel:Parser[GeoModel] = (ident <~ "{") ~ (styleDefinition?) ~ rep(attribute) ~ (rep(geoModel) <~ "}")^^ {
    case name ~ style ~ attr ~ children => GeoModel(name, style, attr, children, diagram)
  }
  def parseRawShape(input:String) = {
    parseAll(shape, input.replaceAll("\\/\\/.+","").split("\n").slice(1, input.lines.length -1).
      map(s => s.trim +"\n").
      mkString).get
  }
}

/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ:String, style:Option[Style], attributes:List[String], children:List[GeoModel], diagram: Diagram){
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
