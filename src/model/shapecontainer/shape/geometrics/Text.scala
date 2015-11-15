package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.geometrics.layouts.{TextLayoutParser, TextLayout}
import model.style.Style
import Alignment._
import util.{CommonParserMethodes, GeoModel}

/**
 * Created by julian on 19.10.15.
 * representation of a text-element
 */
class Text( parent:Option[GeometricModel] = None,
            override val id:String = "",
            textLayout: TextLayout/*textBody (GrammarSheet)*/
            ) extends GeometricModel(parent) with TextLayout with TextBody{
  override val style:Option[Style] = textLayout.style
  override val position:Option[(Int, Int)]= textLayout.position
  override val size_width:Int = textLayout.size_width
  override val size_height:Int = textLayout.size_height
  override val hAlign:Option[HAlign] = textLayout.hAlign
  override val vAlign:Option[VAlign] = textLayout.vAlign
}

abstract class TextType
  case object DefaultText extends TextType
  case object Multiline extends TextType


object Text extends CommonParserMethodes{
  def apply(geoModel:GeoModel, parent:Option[GeometricModel], parentStyle:Option[Style], diagram:Diagram) = parse(geoModel, parent, parentStyle, diagram)
  def parse(geoModel:GeoModel, parent:Option[GeometricModel], parentStyle:Option[Style], diagram:Diagram):Option[Text] = {
    var id:String = ""
    val textLayout:Option[TextLayout] = TextLayoutParser(geoModel, parentStyle, diagram)
    if(textLayout isEmpty)
      return None

    geoModel.attributes.foreach{
      case x if x.matches("id.*") => id = parse(idAsString, x).get
      case _ =>
    }

    if(textLayout.isEmpty || id == "")
      None
    else
      Some(new Text(parent, id, textLayout.get))
  }
}

trait TextBody{
  val id:String
}