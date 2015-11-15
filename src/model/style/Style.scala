package model.style

import model.{ClassHierarchy, Diagram}
import util.CommonParserMethodes

import scala.util.Random

case class Style( name: String = "noName",
                  description: Option[String] = None,
                  transparency: Option[Double] = None,
                  background_color: Option[ColorOrGradient] = None,
                  line_color: Option[Color] = None,
                  line_style: Option[LineStyle] = None,
                  line_width: Option[Int] = None,
                  font_color: Option[ColorOrGradient] = None,
                  font_name: Option[String] = None,
                  font_size: Option[Int] = None,
                  font_bold: Option[Boolean] = None,
                  font_italic: Option[Boolean] = None,
                  gradient_orientation: Option[GradientAlignment] = None,
                  gradient_area_color: Option[ColorOrGradient] = None,
                  gradient_area_offset: Option[Double] = None,
                  selected_highlighting: Option[ColorOrGradient] = None,
                  multiselected_highlighting: Option[ColorOrGradient] = None,
                  allowed_highlighting: Option[ColorOrGradient] = None,
                  unallowed_highlighting: Option[ColorOrGradient] = None,

                  childOf: List[Style] = List()) {
  val key: Long = hashCode
}





/**
 * StyleParser
 * either parses a complete style or just generates an anonymous Style out of only a list of attributes*/
object StyleParser extends CommonParserMethodes {
  val validStyleAttributes = List("description", "transparency", "background-color", "line-color", "line-style", "line-width",
    "font-color", "font-name", "font-size", "font-bold", "font-italic", "gradient-orientation", "gradient-area-color",
    "gradient-area-offset", "highlighting-allowed", "highlighting-unallowed", "highlighting-selected", "highlighting-multiselected")

  private def parseAttributes(input:String) = parse(attributes, input).get

  private def attributes = "style\\s*\\(".r ~> rep(styleAttribute) <~ ")" ^^ {case attr:List[(String, String)] => attr}
  private def styleVariable =("""("""+StyleParser.validStyleAttributes.map(_+"|").mkString+""")""").r ^^ {_.toString}
  private def styleAttribute = styleVariable ~ (styleArguments <~ ",?".r)^^ {case v ~ a => (v, a)}
  private def styleArguments = styleVariable ~> ("=?\\s*".r ~> argument) ^^ {case arg => arg}

  /**
   * Methode for creating a child of type Style, by only giving parentStyles
   * @param diagram only for delegating the actual creation to an apply method of StyleParser
   * @param parents holds all the parentStyles, the returnedsstyle will inherit from
   *                if only one style is given, the given style is returned -> you need two to make an actual child
   * @return s a Option including a new Style or None if no parentstyles were given
   * @note note, that attributes are inherited by the latest bound principle: Style A extends B -> B overrides attributes of A a call like:
   *       StyleParser.makeLove(someDiagram, B, C, D, A) -> A's attributes have highest priority!!
   */
  def makeLove(diagram: Diagram, parents:Option[Style]*):Option[Style] ={
    val parentStyles = parents.filter(_.isDefined)
    if(parentStyles.length == 1) return parentStyles.head
    else if(parentStyles.isEmpty) return None
    val childName = "[child_style]:_"+parentStyles.map(_.get.name+"_").mkString
    Some(StyleParser(childName, Some(parentStyles.toList.map(i => i.get.name)), List[(String, String)](), diagram))
  }

  /**
   * parse
   * @param attributes is the string containing all the information needed to generate the attributes to generate a anonymous Style instance
   */
  def apply(attributes:String) = parse(attributes)
  def parse(attributes:String):Style = {
    var description: Option[String]                        = None
    var transparency: Option[Double]                       = None
    var background_color: Option[ColorOrGradient]          = None
    var line_color: Option[Color]                          = None
    var line_style: Option[LineStyle]                      = None
    var line_width: Option[Int]                            = None
    var font_color: Option[ColorOrGradient]                = None
    var font_name: Option[String]                          = None
    var font_size: Option[Int]                             = None
    var font_bold: Option[Boolean]                         = None
    var font_italic: Option[Boolean]                       = None
    var gradient_orientation: Option[GradientAlignment]    = None
    var gradient_area_color: Option[ColorOrGradient]       = None
    var gradient_area_offset: Option[Double]               = None
    var selected_highlighting: Option[ColorOrGradient]     = None
    var multiselected_highlighting: Option[ColorOrGradient]= None
    var allowed_highlighting: Option[ColorOrGradient]      = None
    var unallowed_highlighting: Option[ColorOrGradient]    = None


    def ifValid[T](f: => T):Option[T] = {
      var ret:Option[T] = None
      try { ret = Some(f)
        ret
      }finally {
        ret
      }}

    val attrList = parseAttributes(attributes)
    if(attrList.nonEmpty){
     attrList.foreach{
       case tuple:(String, String) if tuple._1 == "description" => description = Some(tuple._2)
       case ("transparency", x:String) => transparency = ifValid(x.toDouble)
       case ("background-color", x) => background_color = Some(knownColors.getOrElse(x, GRAY))
       case ("line-color", x) => line_color = Some(knownColors.getOrElse(x, WHITE))
       case ("line-style", x) => line_style= LineStyle.getIfValid(x)
       case ("line-width", x:String) => line_width= ifValid(x.toInt)
       case ("font-color", x) => font_color= Some(knownColors.getOrElse(x, BLACK))
       case ("font-name", x) => font_name= Some(x)
       case ("font-size", x:String) => font_size= ifValid(x.toInt)
       case ("font-bold", x) => font_bold = Some(matchBoolean(x))
       case ("font-italic", x) => font_italic = Some(matchBoolean(x))
       case ("gradient-orientation", x) => gradient_orientation = GradientAlignment.getIfValid(x)
       case ("gradient-area-color", x) => gradient_area_color = Some(knownColors.getOrElse(x, BLACK))
       case ("gradient-area-offset", x:String) => gradient_area_offset= ifValid(x.toDouble)
       case ("highlighting-allowed", x) => allowed_highlighting = Some(knownColors.getOrElse(x, BLUE))
       case ("highlighting-unallowed", x) => unallowed_highlighting= Some(knownColors.getOrElse(x, BLUE))
       case ("highlighting-selected", x) => selected_highlighting = Some(knownColors.getOrElse(x, BLUE))
       case ("highlighting-multiselected", x) => multiselected_highlighting = Some(knownColors.getOrElse(x, BLUE))
     }
    }

    /*create the instance of the actual new Style*/
    Style("anonymousStyle"+Random.nextString(100), description, transparency, background_color, line_color, line_style, line_width, font_color,
      font_name, font_size, font_bold, font_italic, gradient_orientation, gradient_area_color, gradient_area_offset,
      selected_highlighting, multiselected_highlighting, allowed_highlighting, unallowed_highlighting, List())
  }

  /**
   * @param name the name of the ne Style instance
   * @param parents the style instance's names from which the new Style will inherit information
   * @param attributes List of Tuples of Strings -> List[(String, String)] consist of tuple._1 = attribute's name and tuple._2 the according value
   * @param diagram is a Diagram which contains the styleHierarchy which gives information about inheritance*/
  def apply(name:String, parents:Option[List[String]], attributes: List[(String, String)], diagram: Diagram) = parse(name, parents, attributes, diagram)
  def parse(name:String, parents:Option[List[String]], attributes: List[(String, String)], diagram: Diagram):Style ={

    var extendedStyle:List[Style] = List[Style]()

    if(parents.nonEmpty)
      parents.get.foreach{parent => {
        val parentName = parent.replace(",","").trim
        if(diagram.styleHierarchy.contains(parentName))
          extendedStyle = diagram.styleHierarchy(parentName).data :: extendedStyle
        }
      }/*TODO if class was not found, to be inherited tell Logger*/
    /*mapping and defaults*/
    /*fill the "mapping and defaults" with extended information or with None values if necessary*/
    /** relevant is a help-methode, which shortens the actual call to mostRelevant of ClassHierarchy by ensuring the collection-parameter
      * relevant speaks for the hierarchical context -> "A extends B, C" -> C is most relevant */
    def relevant[T](f: Style => Option[T]) = ClassHierarchy.mostRelevant(extendedStyle) {f}

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
    def trimit(arg:String) = arg.replace("=","").trim
    attributes.foreach{
      case x if x._1.trim == "description" => description = Some(trimit(x._2))
      case x if x._1.trim == "transparency" => transparency = Some(trimit(x._2).toDouble)
      case x if x._1.trim.matches("background.?color") => background_color = Some(knownColors.getOrElse(trimit(x._2), GRAY))
      case x if x._1.trim.matches("line[\\-\\_]?color") => line_color = Some(knownColors.getOrElse(trimit(x._2), WHITE))
      case x if x._1.trim.matches("line[\\-\\_]?style") => line_style = LineStyle.getIfValid(trimit(x._2))
      case x if x._1.trim.matches("line[\\-\\_]?width") => line_width = Some(trimit(x._2).toInt)
      case x if x._1.trim.matches("font[\\-\\_]?color") => font_color = Some(knownColors.getOrElse(trimit(x._2), BLACK))
      case x if x._1.trim.matches("font[\\-\\_]?name") => font_name = Some(trimit(x._2))
      case x if x._1.trim.matches("font[\\-\\_]?size") => font_size = Some(trimit(x._2)toInt)
      case x if x._1.trim.matches("font[\\-\\_]?bold") => font_bold = Some(matchBoolean(trimit(x._2)))
      case x if x._1.trim.matches("font[\\-\\_]?italic") => font_italic = Some(matchBoolean(trimit(x._2)))
      case x if x._1.trim.matches("gradient[\\-\\_]?orientation") => gradient_orientation = GradientAlignment.getIfValid(trimit(x._2))
      case x if x._1.trim.contains("gradient-area") => x match {
        case `x` if `x`._1.trim.contains("color") => gradient_area_color = Some(knownColors.getOrElse(trimit(x._2), BLACK))
        case `x` if `x`._1.trim.contains("offset") => gradient_area_offset = Some(trimit(x._2).toDouble)
        case _ => messageIgnored(x._1, name, "Style")
      }
      case x if x._1.trim.contains("highlighting") => x match {
        case `x` if x._1.trim.contains("selected") => selected_highlighting = Some(knownColors.getOrElse(trimit(x._2), BLUE))
        case `x` if x._1.trim.contains("multiselected") => multiselected_highlighting = Some(knownColors.getOrElse(trimit(x._2), BLUE)) /*TODO defaults "BLUE" might not be right*/
        case `x` if x._1.trim.contains("allowed") => allowed_highlighting = Some(knownColors.getOrElse(trimit(x._2), BLUE))
        case `x` if x._1.trim.contains("unallowed") => unallowed_highlighting = Some(knownColors.getOrElse(trimit(x._2), BLUE))
        case _ => messageIgnored(x._1, name, "Style")
      }
      case x => messageIgnored(x._1, name, "Style")
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

  val knownColors = Map(
    "white" -> WHITE,
    "light-light-gray" -> LIGHT_LIGHT_GRAY,
    "light-gray" -> LIGHT_GRAY,
    "gray" -> GRAY,
    "black" -> BLACK,
    "red" -> RED,
    "light-orange" -> LIGHT_ORANGE,
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

  /**
   * states a problem to standardoutput
   * @param attribute is the attribute, that was not matchable
   * @param name is the name of the class
   * @param className is the type (e.G. Style, Shape ...)*/
  def messageIgnored(attribute: String, name: String, className: String) = println("[util.StringToObjectParser|toShape]: attribute -> " +
    attribute + " in " + className + " '" + name + "' was ignored") /*TODO replace with call to Logger*/
}
