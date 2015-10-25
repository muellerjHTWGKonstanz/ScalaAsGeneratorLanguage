package model.shapecontainer.shape.geometrics.layouts

import model.Diagram
import model.style.Style

/**
 * Created by julian on 20.10.15.
 * representation of a RoundedRectangleLayout
 */
trait RoundedRectangleLayout extends CommonLayout{
  val curve_width:Int
  val curve_height:Int
}

object RoundedRectangleLayoutParser {
  def parse(attributes:List[String], diagram:Diagram):Option[RoundedRectangleLayout]={
    val commonLayout = CommonLayoutParser.parse(attributes, diagram)
    if(commonLayout.isEmpty)
      return None

    attributes.foreach{
      case x if x.matches("curve \\((width=)?[0-9]+, (height=)?[0-9]+\\)") => {
        val tup = "[0-9]+".r.findAllIn(x).toArray
        return Some(new RoundedRectangleLayout {
          override val style:Option[Style] = commonLayout.get.style
          override val curve_width: Int = tup(0).toInt
          override val curve_height: Int = tup(1).toInt
          override val position: Option[(Int, Int)] = commonLayout.get.position
          override val size_width: Int = commonLayout.get.size_width
          override val size_height: Int = commonLayout.get.size_height
        })
      }
      case x if x.trim == "}" => return None
    }
    None
  }
}
