package util

import model._
import model.ClassHierarchy._

/**
 * Created by julian on 9/3/15.
 * StringToObjectParser will offer various parsing methods, which convert a string to a style, shape, or spray instance
 */

object StringToObjectParser {

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

  def matchBoolean(b: String): Boolean = b match {
    case `b` if b.matches("yes|true|y") => true
    //case `b` if b.matches("no|false|n") => false
    case _ => false
  }

  /**
   * param string style class in string form to be parsed
   * param diagram for inheritance information*/
  def toStyle(input: String, diagram: Diagram): Style = {
    /*argument splitting*/
    val argArray = input.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val styleHead: Array[String] = argArray(0).split(" ")
    val styleAttributes: Array[String] = if (argArray.size > 1) argArray(1).trim.split("\n") else Array()
    var extendedStyle: List[Style] = List()

    /*check if class extends other class*/
    if (styleHead.contains("extends")) {
      /*look up the extended classes in diagram's classHierarchy and push them on the stack*/
      styleHead.splitAt(3)._2.map(s => s.replace(",", "")).foreach(elem =>
        if(diagram.styleHierarchy.contains(elem)){extendedStyle = diagram.styleHierarchy(elem).data :: extendedStyle})/*TODO if class was not found, to be inherited tell Logger*/
    }

    /*mapping and defaults*/
    /*fill the "mapping and defaults" with extended information or with None values if necessary*/
    val name:String = styleHead(1)
    var key = 0L
    var description: Option[String]                         = mostRelevant(extendedStyle){_.description}
    var transparency: Option[Double]                        = mostRelevant(extendedStyle){_.transparency}
    var background_color: Option[ColorOrGradient]           = mostRelevant(extendedStyle){_.background_color}
    var line_color: Option[Color]                           = mostRelevant(extendedStyle){_.line_color}
    var line_style: Option[LineStyle]                       = mostRelevant(extendedStyle){_.line_style}
    var line_width: Option[Int]                             = mostRelevant(extendedStyle){_.line_width}
    var font_color: Option[ColorOrGradient]                 = mostRelevant(extendedStyle){_.font_color}
    var font_name: Option[String]                           = mostRelevant(extendedStyle){_.font_name}
    var font_size: Option[Int]                              = mostRelevant(extendedStyle){_.font_size}
    var font_bold: Option[Boolean]                          = mostRelevant(extendedStyle){_.font_bold}
    var font_italic: Option[Boolean]                        = mostRelevant(extendedStyle){_.font_italic}
    var gradient_orientation: Option[GradientAlignment]     = mostRelevant(extendedStyle){_.gradient_orientation}
    var gradient_area_color: Option[ColorOrGradient]        = mostRelevant(extendedStyle){_.gradient_area_color}
    var gradient_area_offset: Option[Double]                = mostRelevant(extendedStyle){_.gradient_area_offset}
    var selected_highlighting: Option[ColorOrGradient]      = mostRelevant(extendedStyle){_.selected_highlighting}
    var multiselected_highlighting:Option[ColorOrGradient]  = mostRelevant(extendedStyle){_.multiselected_highlighting}
    var allowed_highlighting:Option[ColorOrGradient]        = mostRelevant(extendedStyle){_.allowed_highlighting}
    var unallowed_highlighting:Option[ColorOrGradient]      = mostRelevant(extendedStyle){_.unallowed_highlighting}

    /*filter the inputString and override attributes accordingly*/
    styleAttributes.foreach { line => line.trim.split(" = ")(0) match {
      case x if x == "key" => key = line.trim.split(" = ")(1).toLong
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
        case _ => messageIgnored(x)
      }
      case x if x.contains("highlighting") => x match {
        case `x` if x.contains("selected") => selected_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case `x` if x.contains("multiselected") => multiselected_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE)) /*TODO defaults "BLUE" might not be right*/
        case `x` if x.contains("allowed") => allowed_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case `x` if x.contains("unallowed") => unallowed_highlighting = Some(knownColors.getOrElse(line.trim.split(" = ")(1), BLUE))
        case _ => messageIgnored(x)
      }
      case x => messageIgnored(x)
    }
    }
    def messageIgnored(attribute: String) = println("[util.StringToObjectParser|toStyleInstance]: attribute -> " +
      attribute + " in style '" + styleHead(1) + "' was ignored")/*TODO replace with call to Logger*/

    /*create the instance of the actual new Style*/
    val newStyle = Style(name, key, description, transparency, background_color, line_color, line_style, line_width, font_color,
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


  def toShape(shape: String, diagram: Diagram): Shape = {
    val argArray = shape.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val shapeHead: Array[String] = argArray(0).split(" ")
    val shapeAttributes: Array[String] = if (argArray.size > 1) argArray(1).trim.split("\n") else Array()

    var extendedShape: Option[Shape] = None

    /*check if shape extends another Shape*/
    if (shapeHead.length > 2 && shapeHead.contains("extends")) {
      extendedShape = Some(diagram.shapeHierarchy(shapeHead(shapeHead.indexOf("extends") + 1)).data)
    }

    /*TODO attributes to be set in new Shape([...])*/
    val name: String = shapeHead(1)
    var key: Long = 0L
    var style: Option[Style] = None

    shapeAttributes.foreach { line => line.trim.split(" = ")(0) match {
      case x if x == "key" => key = line.trim.split(" = ")(1).toLong
      case x: String if x == "style" => style = Some(diagram.styleHierarchy(line.trim.split(" = ")(1)).data)
      case x => println("[util.StringToObjectParser|toShape]: attribute -> " + x + " in shape '" + shapeHead(1) + "' was ignored")
    }
    }

    def getSuperData(parent: Option[Shape]) = diagram.shapeHierarchy(parent.get.name).data

    val ret = Shape(name, key, style match {
      case x: Some[Style] => x
      case None if extendedShape.isDefined => getSuperData(extendedShape).style
      case _ => None
    },
      if (extendedShape.isDefined) extendedShape else None)

    if (extendedShape.isDefined) {
      diagram.shapeHierarchy(extendedShape.get.name, ret)
    } else {
      diagram.shapeHierarchy.newBaseClass(ret)
    }

    ret
  }
}
