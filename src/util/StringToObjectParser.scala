package util

import model._
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.anchor.Anchor
import model.shapecontainer.shape.anchor.Anchor.AnchorType
import model.shapecontainer.shape.geometrics.GeometricModel
import model.style._

/**
 * Created by julian on 9/3/15.
 * StringToObjectParser will offer various parsing methods, which convert a string to a style, shape, or spray instance
 */

object StringToObjectParser {

  /**
   * Takes a given input-String and returns an instance of model.style.Style
   * @param input style class in string form to be parsed
   * @param diagram for inheritance information*/
  def toStyle(input: String, diagram: Diagram): Style = {
    /*argument splitting*/
    val argArray = input.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val styleHead: Array[String] = argArray(0).split(" ")
    val styleAttributes: Array[String] = if (argArray.size > 1) argArray(1).trim.split("\n") else Array()
    var extendedStyle: List[Style] = List[Style]()

    /*check if class extends other class*/
    if (styleHead.contains("extends")) {
      /*look up the extended classes in diagram's classHierarchy and push them on the stack*/
      styleHead.splitAt(3)._2.map(s => s.replace(",", "")).foreach(elem =>
        if (diagram.styleHierarchy.contains(elem)) {
          /*its important to add like this: elem::Tail to keep the most relevant element in the beginning*/
          extendedStyle = diagram.styleHierarchy(elem).data :: extendedStyle
        }) /*TODO if class was not found, to be inherited tell Logger*/
    }

    /*mapping and defaults*/
    /*fill the "mapping and defaults" with extended information or with None values if necessary*/
    /** relevant is a help-methode, which shortens the actual call to mostRelevant of ClassHierarchy by ensuring the collection-parameter
      * relevant speaks for the hierarchical context -> "A extends B, C" -> C is most relevant */
    def relevant[T](f: Style => Option[T]) = ClassHierarchy.mostRelevant(extendedStyle) {
      f
    }

    val name: String = styleHead(1)
    var description: Option[String]                        = relevant { _.description }
    var transparency: Option[Double]                       = relevant { _.transparency }
    var background_color: Option[ColorOrGradient]          = relevant { _.background_color }
    var line_color: Option[Color]                          = relevant { _.line_color }
    var line_style: Option[LineStyle]                      = relevant { _.line_style }
    var line_width: Option[Int]                            = relevant { _.line_width }
    var font_color: Option[ColorOrGradient]                = relevant { _.font_color }
    var font_name: Option[String]                          = relevant { _.font_name }
    var font_size: Option[Int]                             = relevant { _.font_size }
    var font_bold: Option[Boolean]                         = relevant { _.font_bold }
    var font_italic: Option[Boolean]                       = relevant { _.font_italic }
    var gradient_orientation: Option[GradientAlignment]    = relevant { _.gradient_orientation }
    var gradient_area_color: Option[ColorOrGradient]       = relevant { _.gradient_area_color }
    var gradient_area_offset: Option[Double]               = relevant { _.gradient_area_offset }
    var selected_highlighting: Option[ColorOrGradient]     = relevant { _.selected_highlighting }
    var multiselected_highlighting: Option[ColorOrGradient]= relevant { _.multiselected_highlighting }
    var allowed_highlighting: Option[ColorOrGradient]      = relevant { _.allowed_highlighting }
    var unallowed_highlighting: Option[ColorOrGradient]    = relevant { _.unallowed_highlighting }

    /*filter the inputString and override attributes accordingly*/
    styleAttributes.foreach { line => line.trim.split(" = ")(0) match {
      case x if x == "description" => description = Some(line.trim.split(" = ")(1))
      case x if x == "transparency" => transparency = Some(line.trim.split(" = ")(1).toDouble)
      case x if x.matches("background.?color") => background_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), GRAY))
      case x if x.matches("line.?color") => line_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), WHITE))
      case x if x.matches("line.?style") => line_style = LineStyle.getIfValid(line.trim.split(" = ")(1))
      case x if x.matches("line.?width") => line_width = Some(line.trim.split(" = ")(1).toInt)
      case x if x.matches("font.?color") => font_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLACK))
      case x if x.matches("font.?name") => font_name = Some(line.trim.split(" = ")(1))
      case x if x.matches("font.?size") => font_size = Some(line.trim.split(" = ")(1).toInt)
      case x if x.matches("font.?bold") => font_bold = Some(matchBoolean(line.trim.split(" = ")(1)))
      case x if x.matches("font.?italic") => font_italic = Some(matchBoolean(line.trim.split(" = ")(1)))
      case x if x.matches("gradient.?orientation") => gradient_orientation = GradientAlignment.getIfValid(line.trim.split(" = ")(1))
      case x if x.contains("gradient_area") => x match {
        case `x` if x.contains("color") => gradient_area_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLACK))
        case `x` if x.contains("offset") => gradient_area_offset = Some(line.trim.split(" = ")(1).toDouble)
        case _ => messageIgnored(x, name, "Style")
      }
      case x if x.contains("highlighting") => x match {
        case `x` if x.contains("selected") => selected_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case `x` if x.contains("multiselected") => multiselected_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE)) /*TODO defaults "BLUE" might not be right*/
        case `x` if x.contains("allowed") => allowed_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case `x` if x.contains("unallowed") => unallowed_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case _ => messageIgnored(x, name, "Style")
      }
      case x => messageIgnored(x, name, "Style")
    }
    }

    /*create the instance of the actual new Style*/
    val newStyle = Style(name, description, transparency, background_color, line_color, line_style, line_width, font_color,
      font_name, font_size, font_bold, font_italic, gradient_orientation, gradient_area_color, gradient_area_offset,
      selected_highlighting, multiselected_highlighting, allowed_highlighting, unallowed_highlighting, extendedStyle)

    /*include new style instance in stylehierarchie*/
    if (extendedStyle.nonEmpty) {
      extendedStyle.reverse.foreach(elem => diagram.styleHierarchy(elem.name, newStyle))
    } else {
      diagram.styleHierarchy.newBaseClass(newStyle)
    }

    /*return the new Style*/
    newStyle
  }


  /**
   * transforms a given input-String and returns a model.shapecontainer.shape.Shape object
   * @param input is the input String to be converted to a Shape instance
   * @param diagram a Diagram, which includes all the class-hierarchy-information needed, to inherit
   * from parent instances
   * */
  def toShape(input: String, diagram: Diagram): Shape = {
    var allLines:Array[String] = input.split("\n")
    val argArray = input.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val shapeHead: Array[String] = argArray(0).split(" ")
    val shapeAttributes: Array[String] = if (argArray.size > 1) argArray(1).trim.split("\n") else Array()
    var extendedShape: List[Shape] = List[Shape]()

    /*check if class extends other class*/
    if (shapeHead.contains("extends")) {
      /*look up the extended classes in diagram's classHierarchy and push them on the stack*/
      shapeHead.splitAt(3)._2.map(s => s.replace(",", "")).foreach(elem =>
        if (diagram.styleHierarchy.contains(elem)) {
          /*its important to add like this: elem::Tail to keep the most relevant element in the beginning*/
          extendedShape = diagram.shapeHierarchy(elem).data :: extendedShape
        }) /*TODO if class was not found, to be inherited, tell Logger*/
    }

    /*parse all inner GeometricModels and delete their lines out of the all Lines*/
    val geometricModelRegex = "(ellipse|line|point|polygon|polyline|rectangle|roundedrectangle|text) \\{"
    var modelLines:Array[(Int,Int)] = Array[(Int,Int)]()


    /*mapping and defaults*/
    /*fill the "mapping and defaults" with extended information or with None values if necessary*/

    /** relevant is a help-methode, which shortens the actual call to mostRelevant of ClassHierarchy by ensuring the collection-parameter
      * relevant speaks for the hierarchical context -> "A extends B, C" -> C is most relevant */
    def relevant[T](f: Shape => Option[T]) = ClassHierarchy.mostRelevant(extendedShape) {f}

    val name: String = shapeHead(1)
    var style: Option[Style]                   = relevant { _.style }
    var size_width_min: Option[Int]            = relevant { _.size_width_min }
    var size_height_min: Option[Int]           = relevant { _.size_height_min }
    var size_width_max: Option[Int]            = relevant { _.size_width_max }
    var size_height_max: Option[Int]           = relevant { _.size_height_max }
    var stretching_horizontal: Option[Boolean] = relevant { _.stretching_horizontal }
    var stretching_vertical: Option[Boolean]   = relevant { _.stretching_vertical }
    var proportional: Option[Boolean]          = relevant { _.proportional }
    var shape: Option[List[GeometricModel]]    = relevant { _.shape }
    var description: Option[String]            = relevant { _.description } /*TODO strange description in grammar sheet*/
    var anchor: Option[AnchorType]             = relevant { _.anchor }

    shapeAttributes.foreach { line => line.trim.split(" = ")(0) match {
      case x if x == "style" => style = Some(diagram.styleHierarchy(line.trim.split(" = ")(1)).data)
      case x if x.contains("size") => x match {
        case `x` if `x`.contains("width") => `x` match {
          case `x` if `x`.contains("min") => size_width_min = Some(line.trim.split(" = ")(1).toInt)
          case `x` if `x`.contains("max") => size_width_max = Some(line.trim.split(" = ")(1).toInt)
          case _ => messageIgnored(x, name, "Shape")
        }
        case x if x.contains("height")=> x match {
          case `x` if `x`.contains("min") => size_height_min = Some(line.trim.split(" = ")(1).toInt)
          case `x` if `x`.contains("min") => size_height_max = Some(line.trim.split(" = ")(1).toInt)
          case _ => messageIgnored(x, name, "Shape")
        }
      }
      case x if x.contains("stretching") => x match {
        case `x` if `x`.contains("horizontal") => stretching_horizontal = Some(line.trim.split(" = ")(1).toBoolean)
        case `x` if `x`.contains("vertical") => stretching_vertical = Some(line.trim.split(" = ")(1).toBoolean)
        case _ => messageIgnored(x, name, "Shape")
      }
      case x if x == "proportional"     => proportional = Some(matchBoolean(line.trim.split(" = ")(1)))
      case x if x == "description"      => description = Some(line.trim.split(" = ")(1))
      case x if x == "anchor"           => Anchor.getValid(line.trim.split(" = ")(1))
      case x if knownGeometricModels.contains(x) =>parseGeometricModel(x)
      case x => messageIgnored(x, name, "Shape")
    }
    }

    /*create new Shape instance*/
    val newShape = new Shape(name, style, size_width_min, size_height_min, size_width_max,
      size_height_max, stretching_horizontal, stretching_vertical, proportional, shape,
      description, anchor, extendedShape)

    /*include new shape instance in shapehierarchie*/
    if (extendedShape.nonEmpty) {
      extendedShape.reverse.foreach(elem => diagram.shapeHierarchy(elem.name, newShape))
    } else {
      diagram.shapeHierarchy.newBaseClass(newShape)
    }

    /*return new Shape instance*/
    newShape
  }

  val knownColors = Map(
    "white" -> WHITE,
    "light-light-gray" -> LIGHT_LIGHT_GRAY,
    "light-gray" -> LIGHT_GRAY,
    "gray" -> GRAY,
    "black" -> BLACK,
    "red" -> RED,
    "light-orange" -> LIGHt_ORANGE,
    "orange" -> ORANGE,
    "dark-orange" -> DARK_ORANGE,
    "yellow" -> YELLOW,
    "green" -> GREEN,
    "light-green" -> LIGHT_GREEN,
    "dark-green" -> DARK_GREEN,
    "cyan" -> CYAN,
    "light-blue" -> LIGHT_BLUE,
    "blue" -> BLUE,
    "dark-blue" -> DARK_BLUE,
    "transparent" -> Transparent)

  val knownGeometricModels =
    Set("ellipse",
    "Line",
    "Point",
    "Polygon",
    "PolyLine",
    "Rectangle",
    "RoundedRectangle",
    "Text")

  /**
   * takes a String and parses a boolean value out of it -> if string is yes|true|y*/
  def matchBoolean(b: String): Boolean = b match {
    case `b` if b toLowerCase() matches "yes|true|y" => true
    //case `b` if b toLowerCase() matches("no|false|n") => false
    case _ => false
  }

  /**
   * recursively parses a string of the form <geometricModelName> {<attributes>* <geometricModelName>{}*}
   * @param input is a String of the form given above
   * @return a new GeometricModel instance -> can be Ellipse, Line, Polygon etc.*/
  def parseGeometricModel(input:String) = ???/*TODO*/


  /**
   * states a problem to standardoutput
   * @param attribute is the attribute, that was not matchable
   * @param name is the name of the class
   * @param className is the type (e.G. Style, Shape ...)*/
  def messageIgnored(attribute: String, name: String, className: String) = println("[util.StringToObjectParser|toShape]: attribute -> " +
    attribute + " in " + className + " '" + name + "' was ignored") /*TODO replace with call to Logger*/
}
