package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.geometrics.layouts.{TextLayoutParser, TextLayout, CommonLayout}
import model.style.Style
import Alignment._
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * representation of a text-element
 */
class Text( parent:Option[GeometricModel] = None,
            id:String = "",
            textLayout: TextLayout/*textBody (GrammarSheet)*/
            ) extends GeometricModel(parent) with TextLayout{
  override val style:Option[Style] = textLayout.style
  override val position:Option[(Int, Int)]= textLayout.position
  override val size_width:Int = textLayout.size_width
  override val size_height:Int = textLayout.size_height
  override val hAlign:HAlign = textLayout.hAlign
  override val vAlign:VAlign = textLayout.vAlign

}

abstract class TextType
  case object DefaultText extends TextType
  case object Multiline extends TextType


object Text{
  def parse(geoModel:GeoModel, diagram:Diagram, parent:Option[GeometricModel]):Option[Text] = {
    var id:String = ""
    val textLayout:Option[TextLayout] = TextLayoutParser(geoModel.attributes, diagram)

    geoModel.attributes.foreach{
      case x if x.matches("id = (\\w+|[0-9]+)") => id = "(\\w+|[0-9]+)".r.findFirstIn(x).get
    }

    if(textLayout.isEmpty || id == "")
      None
    else
      Some(new Text(parent, id, textLayout.get))
  }
}