package model.shapecontainer.shape.geometrics.compartment

import model.shapecontainer.shape.Shape
import util.CommonParserMethodes

/**
 * Created by julian on 19.10.15.
 * representation of CompartmentInfo
 */
trait CompartmentInfo {
  val compartment_layout:Option[CompartmentLayout] = None
  val compartment_stretching_horizontal:Option[Boolean] = None
  val compartment_stretching_vertical:Option[Boolean] = None
  val compartment_spacing:Option[Int] = None
  val compartment_margin:Option[Int] = None
  val compartment_invisible:Option[Boolean] = None
  val compartment_id:Option[String] = None
}

abstract sealed class CompartmentLayout
  case object FIXED extends CompartmentLayout
  case object VERTICAL extends CompartmentLayout
  case object HORIZONTAL extends CompartmentLayout
  case object FIT extends CompartmentLayout

object CompartmentInfoParser extends CommonParserMethodes {

  def apply(attributes: List[String]): Option[CompartmentInfo] = parse(attributes)
  def parse(attributes: List[String]): Option[CompartmentInfo] = {

    var layout: Option[CompartmentLayout] = None
    var margin: Option[Int] = None
    var spacing: Option[Int] = None
    var stretching_vertical: Option[Boolean] = None
    var stretching_horizontal: Option[Boolean] = None
    var id: Option[String] = None
    var invisible: Option[Boolean] = None

    var compartment_attributes = List[String]()

    attributes.foreach {
      case x if x.startsWith("compartment") =>
        compartment_attributes = this.parse(split_compartment, x).get
      case _ =>
    }

    compartment_attributes.foreach{
      case x if x.startsWith("layout") => layout = Some(parse(parse_layout, x).get)
      case x if x.startsWith("margin") => margin = Some(parse(parse_margin, x).get)
      case x if x.startsWith("spacing") => spacing = Some(parse(parse_spacing, x).get)
      case x if x.startsWith("stretching") =>
        val tup = parse(parse_stretching, x).get
        stretching_horizontal = Some(tup._1)
        stretching_vertical = Some(tup._2)
      case x if x.startsWith("invisible") => invisible = Some(true)
      case x if x.startsWith("id") => id = Some(parse(parse_id, x).get)
    }

    if(layout.isDefined) {
      Some(new CompartmentInfo {
        override val compartment_layout: Option[CompartmentLayout] = layout
        override val compartment_margin: Option[Int] = margin
        override val compartment_spacing: Option[Int] = spacing
        override val compartment_stretching_horizontal: Option[Boolean] = stretching_horizontal
        override val compartment_stretching_vertical: Option[Boolean] = stretching_vertical
        override val compartment_id: Option[String] = id
        override val compartment_invisible: Option[Boolean] = invisible
      })
    }
    else
      None
  }


  def parse_layout = "layout\\s*=\\s*".r ~> "(fixed|vertical|horizontal|fit)".r ^^ {
    case layo if layo == "fixed" => FIXED
    case layo if layo == "horizontal" => HORIZONTAL
    case layo if layo == "vertical" => VERTICAL
    case layo if layo == "fit" => FIT
  }

  def parse_margin = "margin\\s*=\\s*".r ~> "\\d+".r ^^ {
    case mar => mar.toInt
  }
  def parse_spacing = "spacing\\s*=\\s*".r ~> "\\d+".r ^^ {
    case spac => spac.toInt
  }

  def parse_stretching = ("stretching\\s*\\(\\s*horizontal\\s*=".r ~> "(yes|y|true|no|n|false)".r) ~ (",\\s*vertical\\s*=".r ~> ("(yes|y|true|no|n|false)".r <~ ")")) ^^ {
    case hor ~ ver => (matchBoolean(hor), matchBoolean(ver))
  }

  def parse_id = "id\\s*=".r ~> ident ^^ {_.toString}
}

