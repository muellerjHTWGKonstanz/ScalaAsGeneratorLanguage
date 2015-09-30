package util

import java.awt.Color
import model.{Shape, Diagram, Style}

/**
 * Created by julian on 9/3/15.
 * StringToObjectParser will offer various parsing methods, which convert a string to a style, shape, or spray instance
 */

object StringToObjectParser {

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
    val styleAttributes: Array[String] = if(argArray.size > 1)argArray(1).trim.split("\n") else Array()

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
        //case x if x == "name" => name = x
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

    val ret = Style(name, key,
      description match {
        case a: Some[String] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).description
        case _ => None
      },
      transparency match {
        case a: Some[Double] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).transparency
        case _ => None
      },
      background_color match {
        case a: Some[Color] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).backgroundColor
        case _ => None
      },
      line_color match {
        case a: Some[Color] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).lineColor
        case _ => None
      },
      line_style match {
        case a: Some[String] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).lineStyle
        case _ => None
      },
      line_width match {
        case a: Some[Int] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).lineWidth
        case _ => None
      },
      font_color match {
        case a: Some[Color] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).fontColor
        case _ => None
      },
      font_name match {
        case a: Some[String] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).fontName
        case _ => None
      },
      font_size match {
        case a: Some[Int] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).fontSize
        case _ => None
      },
      font_bold match {
        case a: Some[Boolean] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).fontBold
        case _ => None
      },
      font_italic match {
        case a: Some[Boolean] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).fontItalic
        case _ => None
      },
      gradient_orientation match {
        case a: Some[String] => a
        case None if extendedStyle.isDefined => getSuperData(extendedStyle).gradientOrientation
        case _ => None
      },
      extendedStyle)

    if (extendedStyle.isDefined) {
      diagram.styleHierarchy(extendedStyle.get.name, ret)
    } else {
      diagram.styleHierarchy.newBaseClass(ret)
    }
    ret
  }


  def toShape(shape:String, diagram:Diagram): Shape ={
    val argArray = shape.split("\\{|\\}") //to get header (class name) and attributes(foo = bar)
    val shapeHead: Array[String] = argArray(0).split(" ")
    val shapeAttributes: Array[String] = if(argArray.size > 1)argArray(1).trim.split("\n") else Array()

    var extendedShape:Option[Shape] = None

    /*check if shape extends another Shape*/
    if(shapeHead.size > 2 && shapeHead.contains("extends")){
      extendedShape = Some(diagram.shapeHierarchy(shapeHead(shapeHead.indexOf("extends")+1)).data)
    }

    var name:String = shapeHead(1)
    var key:Long = 0L
    var style:Option[Style] = None

    /*TODO convert string body (shapeAttributes) to Option[Attributes]*/
    shapeAttributes.foreach{line => line.trim.split(" = ")(0) match {
      case x:String if x=="style" => style = Some(diagram.styleHierarchy(line.trim.split(" = ")(1)).data)
      case x => println("[util.StringToObjectParser|toShape]: attribute -> " + x + " in shape '" + shapeHead(1) + "' was ignored")
    }}

    def getSuperData(parent: Option[Shape]) = diagram.shapeHierarchy(parent.get.name).data

    val ret = Shape(name, key, style match {
      case x:Some[Style] => x
      case None if extendedShape.isDefined => getSuperData(extendedShape).style
      case _ => None
    },
    if(extendedShape.isDefined)extendedShape else None)

    if (extendedShape.isDefined) {
      diagram.shapeHierarchy(extendedShape.get.name, ret)
    } else {
      diagram.shapeHierarchy.newBaseClass(ret)
    }

    ret
  }
}
