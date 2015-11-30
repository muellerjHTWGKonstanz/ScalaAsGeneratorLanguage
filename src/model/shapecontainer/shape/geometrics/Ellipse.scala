package model.shapecontainer.shape.geometrics

import model.HierarchyContainer
import model.shapecontainer.shape.geometrics.layouts.{CommonLayoutParser, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 20.10.15.
 * representation of an Ellipse (which has the same attributes as a rectangle,
 * thats why it just extends model.shape.geometrics.Rectangle)
 * Vorsicht Ellipse erbt zwar von Rectangle, aber eine Ellipse als ein Rectangle zu benutzen ist nicht der eigentliche Sinn
 * rein pragmatisch, da Ellipse und Rectangle die selben Attribute haben
 */
class Ellipse(parent: Option[GeometricModel] = None,
              commonLayout: CommonLayout,
              compartmentInfo:Option[CompartmentInfo],
              parentOf: List[GeometricModel] = List[GeometricModel]()
               ) extends Rectangle(parent, commonLayout, compartmentInfo, parentOf)

object Ellipse {
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param parent is the parent instance that wraps the new GeometricModel*/
  def apply(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:HierarchyContainer)= parse(geoModel, parent, parentStyle, hierarchyContainer)
  def parse(geoModel: GeoModel, parent: Option[GeometricModel], parentStyle:Option[Style], hierarchyContainer:HierarchyContainer): Option[Ellipse] = {
    /*mapping*/
    val commonLayout: Option[CommonLayout] = CommonLayoutParser.parse(geoModel, parentStyle, hierarchyContainer)
    val compartmentInfo: Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes)

    if (commonLayout.isEmpty)
      return None

    val ret = new Ellipse(parent, commonLayout.get, compartmentInfo)
    ret.children = for (i <- geoModel.children) yield {i.parse(Some(ret), ret.style).get}
    Some(ret)
  }
}
