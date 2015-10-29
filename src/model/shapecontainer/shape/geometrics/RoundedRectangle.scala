package model.shapecontainer.shape.geometrics

import model.Diagram
import model.shapecontainer.shape.geometrics.layouts.{RoundedRectangleLayoutParser, CommonLayoutParser, RoundedRectangleLayout, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * represents a rounded rectangle
 */
class RoundedRectangle(parent:Option[GeometricModel] = None,
                       rrLayout:RoundedRectangleLayout,
                       parentOf:List[GeometricModel] = List[GeometricModel]()
                        ) extends GeometricModel(parent) with RoundedRectangleLayout with Wrapper{
  override val style:Option[Style] = rrLayout.style
  override val curve_width:Int = rrLayout.curve_width/*from RoundedRectangleLayout*/
  override val curve_height:Int= rrLayout.curve_height/*from RoundedRectangleLayout*/
  override val position: Option[(Int, Int)] = rrLayout.position
  override val size_width: Int = rrLayout.size_width
  override val size_height: Int= rrLayout.size_height
  override var children:List[GeometricModel] = parentOf

}

object RoundedRectangle{
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param parent is the parent instance that wraps the new GeometricModel*/
  def apply(geoModel: GeoModel, parent: Option[GeometricModel]) = parse(geoModel, parent)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel]): Option[RoundedRectangle] = {
    /*mapping*/
    val rrLayout: Option[RoundedRectangleLayout] = RoundedRectangleLayoutParser.parse(geoModel)

    if (rrLayout.isEmpty)
      return None


    val ret = new RoundedRectangle(parent, rrLayout.get)
    ret.children = for (i <- geoModel.children) yield {i.parse(Some(ret)).get}
    Some(ret)
  }
}
