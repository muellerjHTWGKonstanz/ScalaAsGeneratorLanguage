package model.shapecontainer.shape.geometrics.layouts

import model.Cashe
import model.shapecontainer.shape.geometrics.Alignment
import model.shapecontainer.shape.geometrics.Alignment.{VAlign, HAlign}
import model.style.Style
import util.{CommonParserMethodes, GeoModel}

/**
 * Created by julian on 20.10.15.
 * representation of a textlayout and its parser
 */
trait TextLayout extends CommonLayout {
  val textBody:String
  val hAlign: Option[HAlign]
  val vAlign: Option[VAlign]
}

object TextLayoutParser extends CommonParserMethodes{
  def apply(geoModel: GeoModel, parentStyle:Option[Style], hierarchyContainer:Cashe): Option[TextLayout] = {
    val attributes = geoModel.attributes

    /*mapping*/
    val commonLayout = CommonLayoutParser.parse(geoModel, parentStyle, hierarchyContainer)
    if (commonLayout.isEmpty)
      return None
    var hali: Option[HAlign] = None
    var vali: Option[VAlign] = None
    var txt = ""

    attributes.foreach {
      case x: String if x.matches("align\\s*\\((horizontal=)?(center|left|right),\\s*(vertical=)?(top|middle|bottom)\\)") =>
        hali = Alignment.parseHAlign("(center|right|left)".r.findFirstIn(x).get)
        vali = Alignment.parseVAlign("(top|middle|bottom)".r.findFirstIn(x).get)
      case x: String if x.matches("(?s)textBody.*") =>
        txt = parse(planeText, x).get
      case _ =>
    }
    Some(new TextLayout {
      override val style: Option[Style] = commonLayout.get.style
      override val textBody = txt
      override val hAlign: Option[HAlign] = hali
      override val vAlign: Option[VAlign] = vali
      override val position: Option[(Int, Int)] = commonLayout.get.position
      override val size_width: Int = commonLayout.get.size_width
      override val size_height: Int = commonLayout.get.size_height
    })
  }

  def planeText:Parser[String] = "textBody" ~> "(?s).*".r ^^ {_.toString}
}
