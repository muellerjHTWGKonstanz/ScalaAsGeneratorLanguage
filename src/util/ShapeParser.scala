package util

import model.Diagram
import model.shapecontainer.shape.geometrics.{Rectangle, GeometricModel}

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by julian on 23.10.15.
 * will be able to parse a shape-string to a Shape instance
 */
class ShapeParser(diagram:Diagram) extends JavaTokenParsers{
  private def variable:Parser[String] = """\w+\s?""".r ^^ {_.toString}
  private def argument_classic:Parser[String] = """=\s?(\d+|\w+)""".r ^^ {_.toString}
  private def argument_advanced_explicit:Parser[String] ="""\((\w+((-|_)\w+)?\s?=\s?(\w+|\d+),?\s?)+\)""".r ^^ {_.toString}
  private def argument_advanced_implicit:Parser[String] ="""\(((\w+|\d+),?\s?)+\)""".r ^^ {_.toString}
  private def argument:Parser[String] = argument_classic | argument_advanced_explicit | argument_advanced_implicit
  private def attribute:Parser[String] = variable ~ argument ^^ {case v ~ arg => v+arg}

  private def geoModel:Parser[GeoModel] = (ident <~ "{") ~ rep(attribute) ~ (rep(geoModel) <~ "}")^^ {
    case name ~ attr ~ children => GeoModel(name, attr, children, diagram)
  }
  private def shape = rep(geoModel)

  def parseRawShape(input:String) = {
    parseAll(shape, input.replaceAll("\\/\\/.+","").split("\n").slice(1, input.lines.length -1).
      map(s => s.trim +"\n").
      mkString)
  }
}

/**
 * GeoModel is a sketch of a GeometricModel, only used for parsing a shape string and temporarily
 * save all the attributes in a struct for later compilation into a GeometricModel*/
case class GeoModel(typ:String, attributes:List[String], children:List[GeoModel], diagram: Diagram){
  def parse(parent:Option[GeometricModel]):GeometricModel = typ match {
      case "rectangle" => Rectangle.parse(this, diagram, None).get
  }
}
