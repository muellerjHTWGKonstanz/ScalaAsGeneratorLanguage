package model.shapecontainer.shape.geometrics

import model.Cashe
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics.layouts.{RoundedRectangleLayoutParser, RoundedRectangleLayout}
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
   * @param parent is the parent instance that wraps the new GeometricModel
   * @param parentStyle is the style used by the parent and eventual will be merged with the geoModels style to a new style
   * @param hierarchyContainer holds hierarchical information about styles and is therefor needed*/
  def apply(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:Cashe, ancestorShape:Shape) = parse(geoModel, parent, parentStyle, hierarchyContainer, ancestorShape)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:Cashe, ancestorShape:Shape): Option[RoundedRectangle] = {
    /*mapping*/
    val rrLayout: Option[RoundedRectangleLayout] = RoundedRectangleLayoutParser.parse(geoModel, parentStyle, hierarchyContainer)

    if (rrLayout.isEmpty)
      return None

    val ret:RoundedRectangle = new RoundedRectangle(parent, rrLayout.get)
    ret.children = for (i <- geoModel.children) yield {
      val re = i.parse(Some(ret), ret.style, ancestorShape)
        re.get
    }
    Some(ret)
  }
}
