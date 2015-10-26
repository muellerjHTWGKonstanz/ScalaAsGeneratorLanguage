package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.shapecontainer.shape.geometrics.Alignment
import model.shapecontainer.shape.geometrics.Alignment.{VAlign, HAlign}
import model.style.Style

/**
 * Created by julian on 20.10.15.
 */
trait TextLayout extends CommonLayout{
  val hAlign:HAlign
  val vAlign:VAlign
}

object TextLayoutParser{
  def apply(attributes:List[String], diagram: Diagram):Option[TextLayout]=parse(attributes, diagram)

  def parse(attributes:List[String], diagram: Diagram):Option[TextLayout]={
    val commonLayout = CommonLayoutParser.parse(attributes, diagram)
    if(commonLayout.isEmpty)
      return None

    attributes.foreach{
      case x:String if x.matches("align \\((horizontal=)?(center|left|right), (vertical=)?(top|middle|bottom)\\)") =>
        val halign = "(center|right|left)".r.findFirstIn(x).get
        val valign = "(top|middle|bottom)".r.findFirstIn(x).get

        return Some(new TextLayout {
          override val style:Option[Style] = commonLayout.get.style
          override val hAlign: HAlign = Alignment.parseHAlign(halign).get
          override val vAlign: VAlign = Alignment.parseVAlign(valign).get
          override val position: Option[(Int, Int)] = commonLayout.get.position
          override val size_width: Int = commonLayout.get.size_width
          override val size_height: Int = commonLayout.get.size_height
        })
    }
    None
  }
}
