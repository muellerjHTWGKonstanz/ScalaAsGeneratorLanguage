package model

case class Style(
                  name:String                             = "noName",
                  key:Long                                = 1L,
                  description:Option[String]              = None,
                  transparency:Option[Double]             = None,
                  background_color:Option[ColorOrGradient] = None,
                  line_color:Option[Color]                 = None,
                  line_style:Option[LineStyle]             = None,
                  line_width:Option[Int]                   = None,
                  font_color:Option[ColorOrGradient]       = None,
                  font_name:Option[String]                 = None,
                  font_size:Option[Int]                    = None,
                  font_bold:Option[Boolean]                = None,
                  font_italic:Option[Boolean]              = None,
                  gradient_orientation:Option[GradientAlignment] = None,
                  gradient_area_color:Option[ColorOrGradient] = None,
                  gradient_area_offset:Option[Double] = None,
                  selected_highlighting:Option[ColorOrGradient] = None,
                  multiselected_highlighting:Option[ColorOrGradient] = None,
                  allowed_highlighting:Option[ColorOrGradient] = None,
                  unallowed_highlighting:Option[ColorOrGradient] = None,

                  childOf:List[Style]= List())

abstract class ColorOrGradient{
  def getRGBValue:String
  def createOpacityValue:String
}

trait Transparency //ColorWithTransparency in grammar Sheet

abstract class Color extends ColorOrGradient with Transparency{
  def createOpacityValue = """1.0"""
}

object Transparent extends Color{
  def getRGBValue = ""
  override def createOpacityValue = """0.0"""
}
abstract class GradientRef extends ColorOrGradient


/*TODO "gradientRef extends ColorOrGradient" from grammar-sheet? unknown types: JvmTypeReference, gradientFromDSL*/
case object WHITE           extends Color{def getRGBValue = """#ffffff"""}
case object LIGHT_LIGHT_GRAY extends Color{def getRGBValue = """#e9e9e9"""}
case object LIGHT_GRAY      extends Color{def getRGBValue = """#d3d3d3"""}
case object GRAY            extends Color{def getRGBValue = """#808080"""}
case object DARK_GRAY       extends Color{def getRGBValue = """#a9a9a9"""}
case object BLACK           extends Color{def getRGBValue = """#000000"""}
case object RED             extends Color{def getRGBValue = """#ff0000"""}
case object LIGHt_ORANGE    extends Color{def getRGBValue = """#ffa07a"""}
case object ORANGE          extends Color{def getRGBValue = """#ffa500"""}
case object DARK_ORANGE     extends Color{def getRGBValue = """#ff8c00"""}
case object YELLOW          extends Color{def getRGBValue = """#ffff00"""}
case object GREEN           extends Color{def getRGBValue = """#008000"""}
case object LIGHT_GREEN     extends Color{def getRGBValue = """#90EE90"""}
case object DARK_GREEN      extends Color{def getRGBValue = """#006400"""}
case object CYAN            extends Color{def getRGBValue = """#00ffff"""}
case object LIGHT_BLUE      extends Color{def getRGBValue = """#add8e6"""}
case object BLUE            extends Color{def getRGBValue = """#0000ff"""}
case object DARK_BLUE       extends Color{def getRGBValue = """#00008b"""}
//TODO case object TRANSPARENT     extends ColorConstant{def getRGBValue = """#"""

  class RGBColor(val red:Int, green:Int, blue:Int) extends Color {
    def getRGBValue = ""+red+green+blue
  }


sealed class GradientAlignment
  case object HORIZONTAL extends GradientAlignment
  case object VERTICAL extends GradientAlignment
object GradientAlignment{
  def getIfValid(s:String) = {
    s match {
      case "horizontal" => Some(HORIZONTAL)
      case "vertical" => Some(VERTICAL)
      case _ => None
    }
  }
}


sealed class LineStyle
  case object SOLID extends LineStyle {def aplply ="solid"}
  case object DOT extends LineStyle {def aplply ="dot"}
  case object DASH extends LineStyle {def aplply ="dash"}
  case object DASHDOT extends LineStyle {def aplply ="dash-dot"}
  case object DASHDOTDOT extends LineStyle {def aplply ="dash-dot-dot"}

object LineStyle{
  def getIfValid(s:String) = {
    s match {
      case "solid" => Some(SOLID)
      case "dot" => Some(DOT)
      case "dash" => Some(DASH)
      case "dashdot" => Some(DASHDOT)
      case "dashdotdot" => Some(DASHDOTDOT)
      case _ => None
    }
  }

}
