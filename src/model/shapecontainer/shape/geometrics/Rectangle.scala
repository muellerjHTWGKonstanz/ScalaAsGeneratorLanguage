package model.shapecontainer.shape.geometrics

import model.HierarchyContainer
import model.shapecontainer.shape.Shape
import model.shapecontainer.shape.geometrics.layouts.{CommonLayoutParser, RectangleEllipeLayout, CommonLayout}
import model.style.Style
import util.GeoModel

/**
 * Created by julian on 19.10.15.
 * representation of  Rectangle
 */
class Rectangle(parent:Option[GeometricModel] = None,
                commonLayout: CommonLayout,
                compartmentInfo:Option[CompartmentInfo],
                parentOf:List[GeometricModel] = List[GeometricModel]()
                 ) extends GeometricModel(parent) with RectangleEllipeLayout with Wrapper with CompartmentInfo{

  override val style:Option[Style] = commonLayout.style
  override val position:Option[(Int, Int)] = commonLayout.position
  override val size_width: Int = commonLayout.size_width
  override val size_height: Int = commonLayout.size_height
  override var children:List[GeometricModel] = parentOf
  override val compartment_layout:Option[CompartmentLayout] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_layout else None
  override val compartment_stretching_horizontal: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_stretching_horizontal else None
  override val compartment_stretching_vertical: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_stretching_vertical else None
  override val compartment_spacing: Option[Int] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_spacing else None
  override val compartment_margin: Option[Int] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_margin else None
  override val compartment_invisible: Option[Boolean] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_invisible else None
  override val compartment_id: Option[String] = if(compartmentInfo isDefined)compartmentInfo.get.compartment_id else None
}

object Rectangle{
  /**
   * parses a GeoModel into an actual GeometricModel, in this case a Rectangle
   * @param geoModel is the sketch to parse into a GeometricModel
   * @param parent is the parent instance that wraps the new GeometricModel*/
  def apply(geoModel:GeoModel, parent:Option[GeometricModel] = None, parentStyle:Option[Style], diagram:HierarchyContainer, ancestorShape:Shape)= parse(geoModel, parent, parentStyle, diagram, ancestorShape)
  def parse(geoModel:GeoModel, parent:Option[GeometricModel] = None, parentStyle:Option[Style], diagram:HierarchyContainer, ancestorShape:Shape): Option[Rectangle] = {
    /*mapping*/
    val commonLayout:Option[CommonLayout] = CommonLayoutParser.parse(geoModel, parentStyle, diagram)
    val compartmentInfo:Option[CompartmentInfo] = CompartmentInfoParser.parse(geoModel.attributes, ancestorShape)

    if(commonLayout.isEmpty)
      return None

    val ret:Rectangle = new Rectangle(parent, commonLayout.get, compartmentInfo, List())
    ret.children = for(i <- geoModel.children)yield{i.parse(Some(ret), ret.style, ancestorShape).get}
    Some(ret)
  }
}
