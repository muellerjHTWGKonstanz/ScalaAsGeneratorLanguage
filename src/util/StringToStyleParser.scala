package util

import java.awt.Color
import model.{Diagram, Style}

/**
 * Created by julian on 9/3/15.
 * StringToObjectParser will offer various parsing methods, which convert a string to a style, shape, or spray instance
 */

object StringToStyleParser {

  val knownColors = Map("green" -> Color.green, "blue" -> Color.blue, "red" -> Color.red, "yellow" -> Color.yellow,
    "orange" -> Color.orange, "black" -> Color.black, "white" -> Color.white, "pink" -> Color.pink, "gray" -> Color.gray)

  def matchBoolean(b: String): Boolean = b match {
    case `b` if b.matches("yes|true|y") => true
    //case `b` if b.matches("no|false|n") => false
    case _ => false
  }

  /**
   * param string style class in string form to be parsed
   * param diagram for inheritance information*/
  def toStyle(string: String, diagram: Diagram): Style = {
    /*argument splitting*/
    val argArray = string.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val styleHead: Array[String] = argArray(0).split(" ")
    val styleAttributes: Array[String] = argArray(1).trim.split("\n")

    //mapping and defaults
    var name = styleHead(1)
    var key = 0L
    var description: Option[String] = None
    var transparency: Option[Double] = None
    var background_color: Option[Color] = None
    var line_color: Option[Color] = None
    var line_style: Option[String] = None /*TODO type?*/
    var line_width: Option[Int] = None
    var font_color: Option[Color] = None
    var font_name: Option[String] = None
    var font_size: Option[Int] = None
    var font_bold: Option[Boolean] = None
    var font_italic: Option[Boolean] = None
    var gradient_orientation: Option[String] = None /*TODO type?*/
    var extendedStyle: Option[Style] = None


    /*find if class extends other class*/
    if (styleHead.contains("extends")) {
      /*look up the extended class in diagram's classHierarchy and get it*/
      extendedStyle = Some(diagram.styleHierarchy(styleHead(styleHead.indexOf("extends") + 1)).data)
    }

    styleAttributes.foreach { line => line.trim.split(" = ")(0) match {
      case x if x == "name" => name = x
      case x if x == "key" => key = line.trim.split(" = ")(1).toLong
      case x if x == "description" => description = Some(line.trim.split(" = ")(1))
      case x if x == "transparency" => transparency = Some(line.trim.split(" = ")(1).toDouble)
      case x if x.matches("background.?color") => background_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), Color.gray))
      case x if x.matches("line.?color") => line_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), Color.white))
      case x if x.matches("line.?style") => line_style = Some(line.trim.split(" = ")(1))
      case x if x.matches("line.?width") => line_width = Some(line.trim.split(" = ")(1).toInt)
      case x if x.matches("font.?color") => font_color = Some(knownColors.getOrElse(line.trim.split(" = ")(1), Color.black))
      case x if x.matches("font.?name") => font_name = Some(line.trim.split(" = ")(1))
      case x if x.matches("font.?size") => font_size = Some(line.trim.split(" = ")(1).toInt)
      case x if x.matches("font.?bold") => font_bold = Some(matchBoolean(line.trim.split(" = ")(1)))
      case x if x.matches("font.?italic") => font_italic = Some(matchBoolean(line.trim.split(" = ")(1)))
      case x if x.matches("gradient.?orientation") => gradient_orientation = Some(line.trim.split(" = ")(1))
      case x => println("[util.StringToObjectParser|toStyleInstance]: attribute -> " + x + " in style '" + styleHead(1) + "' was ignored")
    }
    }

    def getSuperData(parent: Option[Style]) = diagram.styleHierarchy(parent.get.name).data

    val ret = Style(name, key, description, transparency, background_color, line_color, line_style, line_width, font_color,
      font_name, font_size, font_bold, font_italic, gradient_orientation, extendedStyle)

    if (extendedStyle.isDefined) {
      diagram.styleHierarchy(extendedStyle.get.name, ret)
    } else {
      diagram.styleHierarchy.newBaseClass(ret)
    }

    ret
  }
}
